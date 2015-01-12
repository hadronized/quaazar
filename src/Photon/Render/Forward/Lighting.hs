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
import Control.Monad.Trans ( MonadIO(..) )
import Control.Monad.Trans.Either ( EitherT, hoistEither )
import Control.Monad.Trans.Journal ( evalJournalT )
import Data.Bits ( (.|.) )
import Graphics.Rendering.OpenGL.Raw
import Linear
import Numeric.Natural ( Natural )
import Photon.Core.Color ( Color )
import Photon.Core.Material ( Albedo )
import Photon.Render.Camera ( GPUCamera(..) )
import Photon.Render.GL.Framebuffer ( AttachmentPoint(..), Target(..)
                                    , bindFramebuffer )
import Photon.Render.GL.Offscreen
import Photon.Render.GL.Shader ( Uniform, Uniformable, (@=), buildProgram
                               , getUniform, useProgram )
import Photon.Render.GL.Texture ( Format(..), InternalFormat(..)  )
import Photon.Render.Shader ( GPUProgram )
import Photon.Utils.Log

-- |'Lighting' gathers information about lighting in the scene.
data Lighting = Lighting {
    _omniLightProgram :: GPUProgram
  , _lightOff         :: Offscreen
  , _lightUniforms    :: LightingUniforms
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

getLighting :: Natural -> Natural -> EitherT Log IO Lighting
getLighting w h = do
  program <- evalJournalT $ buildProgram lightVS Nothing lightFS <* sinkLogs
  liftIO . print $ Log InfoLog CoreLog "generating light offscreen"
  off <- liftIO (genOffscreen w h RGB32F RGB (ColorAttachment 0) Depth32F DepthAttachment) >>= hoistEither
  uniforms <- liftIO (getLightingUniforms program)
  return (Lighting program off uniforms)

getLightingUniforms :: GPUProgram -> IO LightingUniforms
getLightingUniforms program = do
    useProgram program
    sem "ligDepthmap" >>= (@= (0 :: Int))
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
  useProgram (lighting^.omniLightProgram)
  runCamera gcam projViewU eyeU
  where
    projViewU = unis^.lightCamProjViewU
    eyeU = unis^.lightEyeU
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
  , "  vno = (transpose(inverse(model)) * vec4(no,1.)).xyz;"
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

    -- shadows
 , "  float bias = 0.005;"
  , "  vec3 depthDir = vco - ligPos;"
  , "  float dist = length(depthDir) - bias;"
  , "  float ligDistance = texture(ligDepthmap, depthDir).r;"
  , "  float shadow = 1.;"

  , "  if (ligDistance*ligRad < dist) {"
  , "    shadow = sqrt(ligDistance);"
  , "  }"

    -- final color
  , "  frag = vec4(illum,1.) * shadow;"
  , "}"
  ]
