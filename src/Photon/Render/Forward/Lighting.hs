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

module Photon.Render.Forward.Lighting where

import Control.Applicative
import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Data.Bits ( (.|.) )
import Graphics.Rendering.OpenGL.Raw
import Linear
import Numeric.Natural ( Natural )
import Photon.Core.Color ( Color )
import Photon.Core.Material ( Albedo )
import Photon.Render.Camera ( GPUCamera(..) )
import Photon.Render.GL.Framebuffer ( Target(..), bindFramebuffer )
import Photon.Render.GL.Offscreen
import Photon.Render.GL.Shader ( Program, Uniform, Uniformable, buildProgram
                               , getUniform, unused, useProgram )
import Photon.Render.GL.Texture ( Filter(..), Format(..), InternalFormat(..)  )
import Photon.Utils.Log

-- |'Lighting' gathers information about lighting in the scene.
data Lighting = Lighting {
    _omniLightProgram  :: Program
  , _omniLightUniforms :: OmniLightingUniforms
  , _lightOff          :: Offscreen
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
makeLenses ''OmniLightingUniforms

getLighting :: (Applicative m,MonadIO m,MonadLogger m,MonadError Log m)
            => Natural
            -> Natural
            -> m Lighting
getLighting w h = do
  info CoreLog "generating light offscreen"
  program <- buildProgram omniVS Nothing omniFS <* sinkLogs
  off <- genOffscreen w h Nearest RGB32F RGB
  uniforms <- liftIO (getOmniLightingUniforms program)
  return (Lighting program uniforms off)

getOmniLightingUniforms :: Program -> IO OmniLightingUniforms
getOmniLightingUniforms program = do
    useProgram program
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
  -- omnidirectional lights
  useProgram (lighting^.omniLightProgram)
  runCamera gcam projViewU unused eyeU
  where
    projViewU = unis^.omniLightCamProjViewU
    eyeU = unis^.omniLightEyeU
    unis = lighting^.omniLightUniforms

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
  , "uniform vec3 forward;"
  , "uniform vec3 matDiffAlb;"
  , "uniform vec3 matSpecAlb;"
  , "uniform float matShn;"
  , "uniform vec3 ligPos;"
  , "uniform vec3 ligCol;"
  , "uniform float ligPow;"
  , "uniform float ligRad;"
  , "uniform samplerCube ligDepthmap;"

  , "out vec4 frag;"

  , "void main() {"
  , "  vec3 ligToVertex = ligPos - vco;"
  , "  vec3 ligDir = normalize(ligToVertex);"
  , "  vec3 v = normalize(eye - vco);"
  , "  vec3 r = normalize(reflect(-ligDir,vno));"

    -- lighting
  , "  vec3 diff = max(0.,dot(vno,ligDir)) * ligCol * matDiffAlb;"
  , "  vec3 spec = pow(max(0.,dot(r,v)),matShn) * ligCol * matSpecAlb;"
  , "  float atten = ligPow / (pow(1. + length(ligToVertex)/ligRad,2.));"
  , "  vec3 illum = atten * (diff + spec);"

  , "  frag = vec4(illum,1.);"
  , "}"
  ]
