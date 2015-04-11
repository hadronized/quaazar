-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Quaazar.Render.Lighting where

import Control.Applicative
import Control.Monad ( foldM, void )
import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Graphics.Rendering.OpenGL.Raw
import Data.Traversable ( for )
import Data.Word ( Word32 )
import Linear
import Numeric.Natural ( Natural )
import Foreign hiding ( void )
import Quaazar.Core.Color ( Color(..) )
import Quaazar.Core.Projection ( Projection(Perspective), projectionMatrix )
import Quaazar.Core.Transform
import Quaazar.Core.Light ( Omni(..) )
import Quaazar.Render.GL.Buffer hiding ( MapAccess(..) )
import qualified Quaazar.Render.GL.Buffer as B ( MapAccess(..) )
import Quaazar.Render.GL.Framebuffer as FB ( AttachmentPoint(..), Target(..)
                                           , bindFramebuffer )
import Quaazar.Render.GL.GLObject
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Shader ( Program, Uniform, Uniformable, (@=)
                                , buildProgram, uniform )
import Quaazar.Render.GL.Texture ( Filter(..), Format(..), InternalFormat(..) )
import Quaazar.Render.GLSL
import Quaazar.Render.Light
import Quaazar.Utils.Log

-- |'Lighting' gathers information about lighting in the scene.
data Lighting = Lighting {
    _lightOff        :: Offscreen -- FIXME: destroy that
  , _lightOmniBuffer :: Buffer
  , _shadows         :: Maybe (ShadowConf,Shadows)
  }

data Shadows = Shadows {
    _shadowProgram :: Program
  , _lowShadows    :: CubeOffscreenArray
  , _mediumShadows :: CubeOffscreenArray
  , _highShadows   :: CubeOffscreenArray
  }

makeLenses ''Lighting
makeLenses ''Shadows

-- |@getLighting w h nbMaxLights shadowConf@ creates a 'Lighting' object that
-- can be used later in conjuction with lighting shaders. 'w' and 'h' define
-- the resolution of the render frame. 'nbMaxLights' is a limit used to
getLighting :: (Applicative m,MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
            => Natural
            -> Natural
            -> Natural
            -> Maybe ShadowConf
            -> m Lighting
getLighting w h nbMaxLights shadowConf = do
  info CoreLog "generating lighting"
  off <- genOffscreen w h Nearest RGB32F RGB
  omniBuffer <- genOmniBuffer nbMaxLights
  shadows <- for shadowConf $ \conf -> do
    shadowProg <- buildProgram genShadowmapVS Nothing (Just genShadowmapGS)
      genShadowmapFS
    low <- getShadows (conf^.lowShadowSize) (conf^.lowShadowMaxNb)
    medium <- getShadows (conf^.mediumShadowSize) (conf^.mediumShadowMaxNb)
    high <- getShadows (conf^.highShadowSize) (conf^.highShadowMaxNb)
    return (conf,Shadows shadowProg low medium high)
  return (Lighting off omniBuffer shadows)

getShadows :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m)
           => Natural
           -> Natural
           -> m CubeOffscreenArray
getShadows cubeSize d = genCubeOffscreenArray cubeSize d Nearest RGB32F RGB (ColorAttachment 0) Depth32F Depth DepthAttachment

camProjViewUniform :: Uniform (M44 Float)
camProjViewUniform = uniform camProjViewSem

modelUniform :: Uniform (M44 Float)
modelUniform = uniform modelSem

eyeUniform :: Uniform (V3 Float)
eyeUniform = uniform eyeSem

ligAmbColUniform :: Uniform Color
ligAmbColUniform = uniform ligAmbColSem

ligAmbPowUniform :: Uniform Float
ligAmbPowUniform = uniform ligAmbPowSem

ligOmniNbUniform :: Uniform Word32
ligOmniNbUniform = uniform ligOmniNbSem

-- |ProjView matrix used to generate shadowmaps.
omniProjViews :: Float -> Float -> [M44 Float]
omniProjViews znear radius =
    map ((proj !*!) . completeM33RotMat . fromQuaternion)
      [
        axisAngle yAxis (-pi/2) * axisAngle zAxis pi -- positive x
      , axisAngle yAxis (pi/2) * axisAngle zAxis pi -- negative x
      , axisAngle xAxis (-pi/2) -- positive y
      , axisAngle xAxis (pi/2) -- negative y
      , axisAngle yAxis pi * axisAngle zAxis pi -- positive z
      , axisAngle zAxis (pi) -- negative z
      ]
  where
    proj = projectionMatrix $ Perspective (pi/2) 1 znear radius

-- |Extand a 'M33' matrix to 'M44'.
completeM33RotMat :: M33 Float -> M44 Float
completeM33RotMat (V3 (V3 a b c) (V3 d e f) (V3 g h i)) =
  V4
    (V4 a b c 0)
    (V4 d e f 0)
    (V4 g h i 0)
    (V4 0 0 0 1)

genOmniBuffer :: (MonadScoped IO m,MonadIO m) => Natural -> m Buffer
genOmniBuffer nbLights = do
    buffer <- genObject
    bindBuffer buffer ShaderStorageBuffer
    initBuffer ShaderStorageBuffer bytes
    unbindBuffer ShaderStorageBuffer
    return buffer
  where
    bytes = nbLights * fromIntegral omniBytes

-- WARNING: padding
omniBytes :: Int
omniBytes =
    sizeOf (undefined :: V3 Float) -- transform
    + sizeOf (undefined :: Float) -- float padding
  + sizeOf (undefined :: Color) -- color
  + sizeOf (undefined :: Float) -- power
  + sizeOf (undefined :: Float) -- radius
  + sizeOf (undefined :: Word32) -- shadow lod
  + sizeOf (undefined :: Word32) -- shadowmap index
    + sizeOf (undefined :: Float) -- float padding

-- Poke omnidirectional lights at a given pointer. That pointer should be gotten
-- from the SSBO.
pokeOmnis :: [(Omni,Natural,Natural,Transform)] -> Ptr Word8 -> IO Word32
pokeOmnis omnis ptr = do
    (_,nbLights) <- foldM cache (ptr,0) omnis
    return nbLights
  where
    cache (ptr',nbLights) (omni,shadowLOD,shadowIndex,ent) = do
      writeAt ptr' omni shadowLOD shadowIndex  ent
      return (ptr' `advancePtr` omniBytes,succ nbLights)
    writeAt ptr' (Omni col pw rad _) shadowLOD shadowIndex ent = do
      pokeByteOff ptr' 0 (ent^.transformPosition)
      pokeByteOff ptr' 16 (unColor col)
      pokeByteOff ptr' 28 pw
      pokeByteOff ptr' 32 rad
      pokeByteOff ptr' 36 (fromIntegral shadowLOD :: Word32)
      pokeByteOff ptr' 40 (fromIntegral shadowIndex :: Word32)

-- |Send omnidirectional lights to the GPU.
--
-- The list of omnidirectional lights is a tuple of four objects:
--
--   - the actual 'Omni' light;
--   - the shadow LOD (0 if the light doesn’t cast shadows);
--   - the shadowmap index (whatever if the light doesn’t cast shadows);
--   - the transform of the light.
pushOmnis :: [(Omni,Natural,Natural,Transform)] -> Buffer -> IO ()
pushOmnis omnis omniBuffer = do
  bindBufferAt omniBuffer ShaderStorageBuffer ligOmniSSBOBP
  void . withMappedBuffer ShaderStorageBuffer B.Write $ \ptr -> do
    nbLights <- pokeOmnis omnis ptr
    ligOmniNbUniform @= nbLights

-- |Shadowmap generation vertex shader.
genShadowmapVS :: String
genShadowmapVS = unlines
  [
    "#version 430 core"

  , "layout (location = 0) in vec3 co;"
  , "layout (location = 1) in vec3 no;"

  , "uniform mat4 model;"

  , "void main() {"
  , "  gl_Position = model * vec4(co,1.);"
  , "}"
  ]

-- TODO: instanced geometry shader
-- |Shadowmap generation geometry shader.
--
-- The geometry shader is used because we’re doing a layered rendering in order
-- to generate the whole cube depthmap in one pass. Each primitive (i.e.
-- triangle) gets duplicated 6 times; one time per cubemap face.
genShadowmapGS :: String
genShadowmapGS = unlines
  [
    "#version 430 core"

  , "layout (triangles) in;"
  , "layout (triangle_strip, max_vertices = 18) out;"

  , "out vec3 gco;"

  , "uniform mat4 ligProjViews[6];" -- 6 views
  , "uniform vec3 ligPos;"

  , "void main() {"
  , "  for (int i = 0; i < 6; ++i) {"
  , "    for (int j = 0; j < 3; ++j) {"
  , "      gl_Layer = i;"
  , "      gco = gl_in[j].gl_Position.xyz;"
  , "      gl_Position = ligProjViews[i] * vec4(gl_in[j].gl_Position.xyz - ligPos,1.);"
  , "      EmitVertex();"
  , "    }"
  , "    EndPrimitive();"
  , "  }"
  , "}"
  ]

-- |Shadowmap generation fragment shader.
genShadowmapFS :: String
genShadowmapFS = unlines
  [
    "#version 430 core"

  , "in vec3 gco;"
  , "out float outDistance;"

  , "uniform vec3 ligPos;"
  , "uniform float ligIRad;"

  , "void main() {"
  , "  outDistance = distance(ligPos,gco) * ligIRad;"
  , "}"
  ]
