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
import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Data.Bits ( (.|.) )
import Graphics.Rendering.OpenGL.Raw
import Linear
import Numeric.Natural ( Natural )
import Quaazar.Core.Color ( Color )
import Quaazar.Core.Material ( Albedo )
import Quaazar.Render.Camera ( GPUCamera(..) )
import Quaazar.Render.GL.Framebuffer ( Target(..), bindFramebuffer )
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Shader ( Program, Uniform, Uniformable, buildProgram
                               , getUniform, unused, useProgram )
import Quaazar.Render.GL.Texture ( Filter(..), Format(..), InternalFormat(..)  )
import Quaazar.Utils.Log

-- |'Lighting' gathers information about lighting in the scene.
data Lighting = Lighting {
    _ambientLightProgram  :: Program
  , _ambientLightUniforms :: AmbientLightingUniforms
  , _omniLightProgram     :: Program
  , _omniLightUniforms    :: OmniLightingUniforms
  , _lightOff             :: Offscreen
  }

data AmbientLightingUniforms = AmbientLightingUniforms {
    _ambientLightCamProjViewU :: Uniform (M44 Float)
  , _ambientLightModelU       :: Uniform (M44 Float)
  , _ambientLightMatDiffAlbU  :: Uniform Albedo
  , _ambientLightColU         :: Uniform Color
  , _ambientLightPowU         :: Uniform Float
  }

data OmniLightingUniforms = OmniLightingUniforms {
    _omniLightCamProjViewU :: Uniform (M44 Float)
  , _omniLightModelU       :: Uniform (M44 Float)
  , _omniLightEyeU         :: Uniform (V3 Float)
  , _omniLightMatDiffAlbU  :: Uniform Albedo
  , _omniLightMatSpecAlbU  :: Uniform Albedo
  , _omniLightMatShnU      :: Uniform Float
  , _omniLightPosU         :: Uniform (V3 Float) -- FIXME: github issue #22
  , _omniLightColU         :: Uniform Color
  , _omniLightPowU         :: Uniform Float
  , _omniLightRadU         :: Uniform Float
  }

makeLenses ''Lighting
makeLenses ''AmbientLightingUniforms
makeLenses ''OmniLightingUniforms

getLighting :: (Applicative m,MonadIO m,MonadLogger m,MonadError Log m)
            => Natural
            -> Natural
            -> m Lighting
getLighting w h = do
  info CoreLog "generating lighting"
  ambientProgram <- buildProgram ambientVS Nothing ambientFS
  ambientUniforms <- liftIO (getAmbientLightingUniforms ambientProgram)
  omniProgram <- buildProgram omniVS Nothing omniFS
  omniUniforms <- liftIO (getOmniLightingUniforms omniProgram)
  off <- genOffscreen w h Nearest RGB32F RGB
  return (Lighting ambientProgram ambientUniforms omniProgram omniUniforms off)

getAmbientLightingUniforms :: Program -> IO AmbientLightingUniforms
getAmbientLightingUniforms program = do
    AmbientLightingUniforms
      <$> sem "projView"
      <*> sem "model"
      <*> sem "matDiffAlb"
      <*> sem "ligCol"
      <*> sem "ligPow"
  where
    sem :: (Uniformable a) => String -> IO (Uniform a)
    sem = getUniform program

getOmniLightingUniforms :: Program -> IO OmniLightingUniforms
getOmniLightingUniforms program = do
    useProgram program -- FIXME: not mandatory
    OmniLightingUniforms
      <$> sem "projView"
      <*> sem "model"
      <*> sem "eye"
      <*> sem "matDiffAlb"
      <*> sem "matSpecAlb"
      <*> sem "matShn"
      <*> sem "ligPos"
      <*> sem "ligCol"
      <*> sem "ligPow"
      <*> sem "ligRad"
  where
    sem :: (Uniformable a) => String -> IO (Uniform a)
    sem = getUniform program

purgeLightingFramebuffer :: Lighting -> IO ()
purgeLightingFramebuffer lighting = do
  bindFramebuffer (lighting^.lightOff.offscreenFB) ReadWrite
  glClearColor 0 0 0 0
  glClear $ gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT

pushCameraToLighting :: Lighting -> GPUCamera -> IO ()
pushCameraToLighting lighting gcam = do
  -- ambient lights
  useProgram (lighting^.ambientLightProgram)
  runCamera gcam (ambientUnis^.ambientLightCamProjViewU) unused unused
  -- omnidirectional lights
  useProgram (lighting^.omniLightProgram)
  runCamera gcam (omniUnis^.omniLightCamProjViewU) unused (omniUnis^.omniLightEyeU)
  where
    ambientUnis = lighting^.ambientLightUniforms
    omniUnis = lighting^.omniLightUniforms

ambientVS :: String
ambientVS = unlines
  [
    "#version 330 core"

  , "layout (location = 0) in vec3 co;"
  -- , "layout (location = 1) in vec3 no;" -- FIXME: not sure

  , "uniform mat4 projView;"
  , "uniform mat4 model;"

  , "void main() {"
  , "  vec3 vco = (model * vec4(co,1.)).xyz;"
  , "  gl_Position = projView * vec4(vco,1.);"
  , "}"
  ]

ambientFS :: String
ambientFS = unlines
  [
    "#version 330 core"

  , "in vec3 vco;"
  , "in vec3 vno;"

  , "uniform vec3 matDiffAlb;"
  , "uniform vec3 ligCol;"
  , "uniform float ligPow;"

  , "out vec4 frag;"

  , "void main() {"
  , "  frag = vec4(ligCol * matDiffAlb * ligPow,1.);"
  , "}"
  ]

omniVS :: String
omniVS = unlines
  [
    "#version 330 core"

  , "layout (location = 0) in vec3 co;"
  , "layout (location = 1) in vec3 no;"

  , "uniform mat4 projView;"
  , "uniform mat4 model;"

  , "out vec3 vco;"
  , "out vec3 vno;"

  , "void main() {"
  , "  vco = (model * vec4(co,1.)).xyz;"
  , "  vno = (transpose(inverse(model)) * vec4(no,1.)).xyz;"
  , "  gl_Position = projView * vec4(vco,1.);"
  , "}"
  ]

omniFS :: String
omniFS = unlines
  [
    "#version 330 core"

  , "in vec3 vco;"
  , "in vec3 vno;"

  , "uniform vec3 eye;"
  , "uniform vec3 matDiffAlb;"
  , "uniform vec3 matSpecAlb;"
  , "uniform float matShn;"
  , "uniform vec3 ligPos;"
  , "uniform vec3 ligCol;"
  , "uniform float ligPow;"
  , "uniform float ligRad;"

  , "out vec4 frag;"

  , "void main() {"
  , "  vec3 ligToVertex = ligPos - vco;"
  , "  vec3 ligDir = normalize(ligToVertex);"
  , "  vec3 v = normalize(eye - vco);"
  , "  vec3 r = normalize(reflect(-ligDir,vno));"
  , "  vec3 diff = max(0.,dot(vno,ligDir)) * ligCol * matDiffAlb;"
  , "  vec3 spec = pow(max(0.,dot(r,v)),matShn) * ligCol * matSpecAlb;"
  , "  float atten = ligPow / (pow(1. + length(ligToVertex)/ligRad,2.));"
  , "  vec3 illum = atten * (diff + spec);"

  , "  frag = vec4(illum,1.);"
  , "}"
  ]
