-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Quaazar.Render.Forward.Lighting where

import Control.Applicative
import Control.Monad ( foldM )
import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Data.Bits ( (.|.) )
import Data.Word ( Word8, Word32 )
import Graphics.Rendering.OpenGL.Raw
import Linear
import Numeric.Natural ( Natural )
import Foreign
import Quaazar.Core.Color ( Color(..) )
import Quaazar.Core.Entity ( Entity, entityPosition )
import Quaazar.Core.Light ( Omni(..) )
import Quaazar.Core.Material ( Albedo )
import Quaazar.Render.Camera ( GPUCamera(..) )
import Quaazar.Render.GL.Buffer hiding ( MapAccess(..) )
import qualified Quaazar.Render.GL.Buffer as B ( MapAccess(..) )
import Quaazar.Render.GL.Framebuffer as FB ( Target(..), bindFramebuffer )
import Quaazar.Render.GL.GLObject
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Shader ( Program, Uniform, Uniformable, (@=)
                                , buildProgram, getUniform, unused, useProgram
                                , uniform )
import Quaazar.Render.GL.Texture ( Filter(..), Format(..), InternalFormat(..)  )
import Quaazar.Utils.Log

-- |'Lighting' gathers information about lighting in the scene.
data Lighting = Lighting {
    _lightProgram     :: Program
  , _lightUniforms    :: LightingUniforms
  , _lightOff         :: Offscreen
  , _lightOmniBuffer  :: Buffer
  }

data LightingUniforms = LightingUniforms {
    _lightCamProjViewU :: Uniform (M44 Float)
  , _lightModelU       :: Uniform (M44 Float)
  , _lightEyeU         :: Uniform (V3 Float)
  , _lightMatDiffAlbU  :: Uniform Albedo
  , _lightMatSpecAlbU  :: Uniform Albedo
  , _lightMatShnU      :: Uniform Float
  , _lightLigAmbCol    :: Uniform Color
  , _lightLigAmbPow    :: Uniform Float
  , _lightLigOmniNb    :: Uniform Word32
  }

makeLenses ''Lighting
makeLenses ''LightingUniforms

getLighting :: (Applicative m,MonadIO m,MonadLogger m,MonadError Log m)
            => Natural
            -> Natural
            -> Natural
            -> m Lighting
getLighting w h nbLights = do
  info CoreLog "generating lighting"
  program <- buildProgram lightVS Nothing lightFS
  uniforms <- liftIO (getLightingUniforms program)
  off <- genOffscreen w h Nearest RGB32F RGB
  omniBuffer <- liftIO (genOmniBuffer nbLights)
  return (Lighting program uniforms off omniBuffer)

getLightingUniforms :: Program -> IO LightingUniforms
getLightingUniforms program = do
    useProgram program -- FIXME: not mandatory
    LightingUniforms
      <$> sem "projView"
      <*> sem "model"
      <*> sem "eye"
      <*> sem "matDiffAlb"
      <*> sem "matSpecAlb"
      <*> sem "matShn"
      <*> pure (uniform $ fromIntegral ligAmbColSem)
      <*> pure (uniform $ fromIntegral ligAmbPowSem)
      <*> pure (uniform $ fromIntegral ligOmniNbSem)
  where
    sem :: (Uniformable a) => String -> IO (Uniform a)
    sem = getUniform program

purgeLightingFramebuffer :: Lighting -> IO ()
purgeLightingFramebuffer lighting = do
  bindFramebuffer (lighting^.lightOff.offscreenFB) FB.ReadWrite
  glClearColor 0 0 0 0
  glClear $ gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT

pushCameraToLighting :: Lighting -> GPUCamera -> IO ()
pushCameraToLighting lighting gcam = do
  useProgram (lighting^.lightProgram)
  runCamera gcam (unis^.lightCamProjViewU) unused (unis^.lightEyeU)
  where
    unis = lighting^.lightUniforms

genOmniBuffer :: Natural -> IO Buffer
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
  + sizeOf (undefined :: Color)
    + sizeOf (undefined :: Float) -- float padding
  + sizeOf (undefined :: Float) -- power
  + sizeOf (undefined :: Float) -- radius
    + sizeOf (undefined :: V2 Float) -- vec2 padding

-- Poke omnidirectional lights at a given pointer. That pointer should be gotten
-- from the SSBO.
pokeOmnis :: [(Omni,Entity)] -> Ptr Word8 -> IO Word32
pokeOmnis omnis ptr = do
    (_,nbLights) <- foldM cache (ptr,0) omnis
    return nbLights
  where
    cache (ptr,nbLights) (omni,ent) = do
      writeAt ptr omni ent
      return (ptr `advancePtr` omniBytes,succ nbLights)
    writeAt ptr (Omni col pow rad _) ent = do
      pokeByteOff ptr 0 (ent^.entityPosition)
      pokeByteOff ptr 16 (unColor col)
      pokeByteOff ptr 32 pow
      pokeByteOff ptr 36 rad

pushOmnis :: [(Omni,Entity)] -> Lighting -> IO ()
pushOmnis omnis lighting = do
    bindBufferAt (lighting^.lightOmniBuffer) ShaderStorageBuffer ligOmniSSBOBP
    void . withMappedBuffer ShaderStorageBuffer B.Write $ \ptr -> do
      nbLights <- pokeOmnis omnis ptr
      lighting^.lightUniforms.lightLigOmniNb @= nbLights

lightVS :: String
lightVS = unlines
  [
    "#version 430 core"

  , "layout (location = 0) in vec3 co;"
  , "layout (location = 1) in vec3 no;"

  , "uniform mat4 projView;"
  , "uniform mat4 model;"

  , "out vec3 vco;"
  , "out vec3 vno;"

  , "void main() {"
  , "  vco = (model * vec4(co,1.)).xyz;"
  , "  vno = normalize((transpose(inverse(model)) * vec4(no,1.)).xyz);"
  , "  gl_Position = projView * vec4(vco,1.);"
  , "}"
  ]

lightFS :: String
lightFS = unlines
  [
    "#version 430 core"

  , "in vec3 vco;"
  , "in vec3 vno;"

  , "uniform vec3 eye;"
  , "uniform vec3 matDiffAlb;"
  , "uniform vec3 matSpecAlb;"
  , "uniform float matShn;"
    -- ambient lighting
  , declUniform ligAmbColSem "vec3 ligAmbCol"
  , declUniform ligAmbPowSem "float ligAmbPow"
    -- omni lights
  , "struct Omni {"
  , "  vec3 pos;"
  , "  vec3 col;"
  , "  float pow;"
  , "  float rad;"
  , " };"

  , declUniformBlock ligOmniSSBOBP "OmniBuffer { Omni omnis[]; }"
  , declUniform ligOmniNbSem "uint ligOmniNb"

  , "out vec4 frag;"

  , "void main() {"
  , "  vec3 v = normalize(eye - vco);"

    -- ambient lighting
  , "  vec3 ambient = ligAmbCol * matDiffAlb * ligAmbPow;"

    -- omni lights
  , "  vec3 omni = vec3(0.,0.,0.);"
  , "  for (uint i = 0; i < ligOmniNb; ++i) {"
  , "    vec3 ligCol = omnis[i].col;"
  , "    float ligPow = omnis[i].pow;"
  , "    float ligRad = omnis[i].rad;"
  , "    vec3 ligToVertex = omnis[i].pos - vco;"
  , "    vec3 ligDir = normalize(ligToVertex);"
  , "    vec3 r = normalize(reflect(-ligDir,vno));"
  , "    vec3 diff = max(0.,dot(vno,ligDir)) * ligCol * matDiffAlb;"
  , "    vec3 spec = pow(max(0.,dot(r,v)),matShn) * ligCol * matSpecAlb;"
  , "    float atten = ligPow / (pow(1. + length(ligToVertex)/ligRad,2.));"
  , "    omni += atten * (diff + spec);"
  , "  }"

  , "  frag = vec4(ambient + omni,1.);"
  , "}"
  ]

--------------------------------------------------------------------------------
-- GLSL SEMANTICS
declUniform :: Int -> String -> String
declUniform s n = "layout (location = " ++ show s ++ ") uniform " ++ n ++ ";"

ligAmbColSem :: Int
ligAmbColSem = 5

ligAmbPowSem :: Int
ligAmbPowSem = 6

ligOmniNbSem :: Int
ligOmniNbSem = 7

--------------------------------------------------------------------------------
-- GLSL BINDING POINTS
declUniformBlock :: Natural -> String -> String
declUniformBlock bp block = "layout (std430,binding = " ++ show bp ++ ") buffer " ++ block ++ ";"

ligOmniSSBOBP :: Natural
ligOmniSSBOBP = 0
