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
    _lightProgram     :: Program
  , _lightUniforms    :: LightingUniforms
  , _lightOff         :: Offscreen
  }

data LightingUniforms = LightingUniforms {
    _lightCamProjViewU :: Uniform (M44 Float)
  , _lightModelU       :: Uniform (M44 Float)
  , _lightEyeU         :: Uniform (V3 Float)
  , _lightMatDiffAlbU  :: Uniform Albedo
  , _lightMatSpecAlbU  :: Uniform Albedo
  , _lightMatShnU      :: Uniform Float
  , _lightPosU         :: Uniform (V3 Float) -- FIXME: github issue #22
  , _lightColU         :: Uniform Color
  , _lightPowU         :: Uniform Float
  , _lightRadU         :: Uniform Float
  }

makeLenses ''Lighting
makeLenses ''LightingUniforms

getLighting :: (Applicative m,MonadIO m,MonadLogger m,MonadError Log m)
            => Natural
            -> Natural
            -> m Lighting
getLighting w h = do
  info CoreLog "generating lighting"
  program <- buildProgram lightVS Nothing lightFS
  uniforms <- liftIO (getLightingUniforms program)
  off <- genOffscreen w h Nearest RGB32F RGB
  return (Lighting program uniforms off)

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
  useProgram (lighting^.lightProgram)
  runCamera gcam (unis^.lightCamProjViewU) unused (unis^.lightEyeU)
  where
    unis = lighting^.lightUniforms

lightVS :: String
lightVS = unlines
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
  , "  vno = normalize((transpose(inverse(model)) * vec4(no,1.)).xyz);"
  , "  gl_Position = projView * vec4(vco,1.);"
  , "}"
  ]

lightFS :: String
lightFS = unlines
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
