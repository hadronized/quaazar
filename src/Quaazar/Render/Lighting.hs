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

import Control.Monad ( foldM, void )
import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Data.Foldable ( for_, traverse_ )
import Data.Traversable ( for )
import Linear
import Numeric.Natural ( Natural )
import Foreign hiding ( void )
import Quaazar.Render.GL.Buffer hiding ( MapAccess(..) )
import qualified Quaazar.Render.GL.Buffer as B ( MapAccess(..) )
import Quaazar.Render.GL.Framebuffer as FB ( AttachmentPoint(..), Target(..)
                                           , bindFramebuffer )
import Quaazar.Render.GL.GLObject
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Shader ( Program, Uniform, (@=), buildProgram
                                , useProgram )
import Quaazar.Render.GL.Texture ( CubemapArray, Filter(..), InternalFormat(..)
                                 , Unit )
import Quaazar.Render.Light
import Quaazar.Render.Projection ( Projection(Perspective), projectionMatrix )
import Quaazar.Render.Semantics
import Quaazar.Render.Viewport
import Quaazar.Lighting.Light ( Omni(..) )
import Quaazar.Lighting.Shadow ( ShadowLOD(..) )
import Quaazar.Render.Mesh ( GPUMesh, renderMesh )
import Quaazar.Scene.Color ( Color(..) )
import Quaazar.Scene.Hierarchy ( Instance, instCarried, instTransform )
import Quaazar.Scene.Transform
import Quaazar.Utils.Log

-- |'Lighting' gathers information about lighting in the scene.
data Lighting = Lighting {
    _lightOff        :: Offscreen -- FIXME: destroy that
  , _lightOmniBuffer :: Buffer
  , _shadows         :: Maybe (ShadowConf,Shadows)
  }

data Shadows = Shadows {
    _shadowProgram   :: Program
  , _lowShadows      :: (CubeOffscreenArray,Viewport)
  , _mediumShadows   :: (CubeOffscreenArray,Viewport)
  , _highShadows     :: (CubeOffscreenArray,Viewport)
  }

makeLenses ''Lighting
makeLenses ''Shadows

-- |@getLighting w h nbMaxLights shadowConf@ creates a 'Lighting' object that
-- can be used later in conjuction with lighting shaders. 'w' and 'h' define
-- the resolution of the render frame. 'nbMaxLights' is a limit used to
getLighting :: (Applicative m,MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m)
            => Float
            -> Float
            -> Natural
            -> Natural
            -> Natural
            -> Maybe ShadowConf
            -> m Lighting
getLighting znear zfar w h nbMaxLights shadowConf = do
  info CoreLog "generating lighting"
  off <- genOffscreen w h Nearest RGB32F
  omniBuffer <- genOmniBuffer nbMaxLights
  shdws <- for shadowConf $ \conf -> do
    shadowProg <- genShadowProgram znear zfar
    low <- getShadows (conf^.lowShadowSize) (conf^.lowShadowMaxNb)
    medium <- getShadows (conf^.mediumShadowSize) (conf^.mediumShadowMaxNb)
    high <- getShadows (conf^.highShadowSize) (conf^.highShadowMaxNb)
    return (conf,Shadows shadowProg low medium high)
  return (Lighting off omniBuffer shdws)

genShadowProgram :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m)
                 => Float
                 -> Float
                 -> m Program
genShadowProgram znear zfar = do
  program <- buildProgram genShadowmapVS Nothing (Just genShadowmapGS) genShadowmapFS
  liftIO $ do
    useProgram program
    ligProjViewsUniform @= omniProjViews znear zfar
  return program

getShadows :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m)
           => Natural
           -> Natural
           -> m (CubeOffscreenArray,Viewport)
getShadows cubeSize d =
  (,)
    <$> genCubeOffscreenArray cubeSize d Linear R32F (ColorAttachment 0) Depth32F DepthAttachment
    <*> pure (Viewport 0 0 cubeSize cubeSize)

camProjViewUniform :: Uniform (M44 Float)
camProjViewUniform = toUniform CamProjViewSem

modelUniform :: Uniform (M44 Float)
modelUniform = toUniform ModelSem

eyeUniform :: Uniform (V3 Float)
eyeUniform = toUniform EyeSem

ligAmbColUniform :: Uniform Color
ligAmbColUniform = toUniform LigAmbColSem

ligAmbPowUniform :: Uniform Float
ligAmbPowUniform = toUniform LigAmbPowSem

ligOmniNbUniform :: Uniform Word32
ligOmniNbUniform = toUniform LigOmniNbSem

ligProjViewsUniform :: Uniform [M44 Float]
ligProjViewsUniform = toUniform LigProjViewsSem

ligPosUniform :: Uniform (V3 Float)
ligPosUniform = toUniform LigPosSem

ligIRadUniform :: Uniform Float
ligIRadUniform = toUniform LigIRadSem

shadowmapIndexUniform :: Uniform Word32
shadowmapIndexUniform = toUniform ShadowmapIndexSem

lowShadowmapsUniform :: Uniform (CubemapArray,Unit)
lowShadowmapsUniform = toUniform LowShadowmapsSem

mediumShadowmapsUniform :: Uniform (CubemapArray,Unit)
mediumShadowmapsUniform = toUniform MediumShadowmapsSem

highShadowmapsUniform :: Uniform (CubemapArray,Unit)
highShadowmapsUniform = toUniform HighShadowmapsSem

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
      , axisAngle zAxis pi -- negative z
      ]
  where
    proj = projectionMatrix $ Perspective (pi/2) 1 znear radius

-- |Extend a 'M33' matrix to 'M44'.
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
pokeOmnis :: [(Omni,Natural,Transform)] -> Ptr Word8 -> IO Word32
pokeOmnis omnis ptr = do
    (_,nbLights) <- foldM cache (ptr,0) omnis
    return nbLights
  where
    cache (ptr',nbLights) (omni,shadowIndex,trsf) = do
      writeAt ptr' omni shadowIndex  trsf
      return (ptr' `advancePtr` omniBytes,succ nbLights)
    writeAt ptr' (Omni col pw rad shadowLOD) shadowIndex trsf = do
      pokeByteOff ptr' 0 (trsf^.transformPosition)
      pokeByteOff ptr' 16 (unColor col)
      pokeByteOff ptr' 28 pw
      pokeByteOff ptr' 32 rad
      pokeByteOff ptr' 36 $ case shadowLOD of
        Nothing -> 0 :: Word32
        Just lod -> case lod of
          LowShadow    -> 1
          MediumShadow -> 2
          HighShadow   -> 3
      pokeByteOff ptr' 40 (fromIntegral shadowIndex :: Word32)

-- |Send omnidirectional lights to the GPU.
--
-- The list of omnidirectional lights is a tuple of four objects:
--
--   - the actual 'Omni' light;
--   - the shadow LOD (0 if the light doesn’t cast shadows);
--   - the shadowmap index (whatever if the light doesn’t cast shadows);
--   - the transform of the light.
pushOmnis :: [(Omni,Natural,Transform)] -> Buffer -> IO ()
pushOmnis omnis omniBuffer = do
  bindBufferAt omniBuffer ShaderStorageBuffer ligOmniSSBOBP
  void . withMappedBuffer ShaderStorageBuffer B.Write $ \ptr -> do
    nbLights <- pokeOmnis omnis ptr
    ligOmniNbUniform @= nbLights

-- |Generate the shadowmap for a given light and given objects to render. If the
-- light doesn’t cast shadows, do nothing.
--
-- This function doesn’t perform any kind of culling on the objects. You should
-- cull them before calling that function to maximize performance.
genShadowmap :: Omni -> Natural -> Transform -> [Instance GPUMesh] -> Shadows -> IO ()
genShadowmap (Omni _ _ rad shadowLOD) shadowmapIndex lightTrsf meshes shdws =
    for_ shadowLOD $ \lod -> do
      let shadow = case lod of
            LowShadow -> shdws^.lowShadows
            MediumShadow -> shdws^.mediumShadows
            HighShadow -> shdws^.highShadows
      bindFramebuffer (fst shadow ^. cubeOffscreenArrayFB) ReadWrite
      setViewport (snd shadow)
      useProgram (shdws^.shadowProgram)
      ligPosUniform @= lightTrsf^.transformPosition
      shadowmapIndexUniform @= (fromIntegral shadowmapIndex :: Word32)
      ligIRadUniform @= 1 / rad
      traverse_ renderMesh_ meshes
  where
    renderMesh_ meshInst = do
      let
        gmesh = instCarried meshInst
        meshTrsf = instTransform meshInst
      renderMesh gmesh modelUniform meshTrsf

-- |Shadowmap generation vertex shader.
genShadowmapVS :: String
genShadowmapVS = unlines
  [
    "#version 430 core"

  , declInput CoInput "vec3 co"
  , declInput NoInput "vec3 no"

  , declUniform ModelSem "mat4 model"

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

  , declUniform LigProjViewsSem "mat4 ligProjViews[6]"
  , declUniform LigPosSem "vec3 ligPos"
  , declUniform ShadowmapIndexSem "uint shadowmapIndex"

  , "void main() {"
  , "  for (uint layerFaceID = shadowmapIndex * 6, faceID = 0; faceID < 6; ++layerFaceID, ++faceID) {"
  , "    for (uint i = 0u; i < 3u; ++i) {"
  , "      gl_Layer = int(layerFaceID);"
  , "      gco = gl_in[i].gl_Position.xyz;"
  , "      gl_Position = ligProjViews[faceID] * vec4(gco - ligPos,1.);"
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

  , declUniform LigPosSem "vec3 ligPos"
  , declUniform LigIRadSem "float ligIRad"

  , "void main() {"
  , "  outDistance = distance(ligPos,gco) * ligIRad;"
  , "}"
  ]
