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

module Photon.Render.Forward.Shadowing where

import Control.Applicative
import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Data.Bits ( (.|.) )
import Graphics.Rendering.OpenGL.Raw
import Linear
import Numeric.Natural ( Natural )
import Photon.Render.GL.Framebuffer ( AttachmentPoint(..), Target(..)
                                    , bindFramebuffer )
import Photon.Render.GL.Offscreen
import Photon.Render.GL.Shader ( Uniform, Uniformable, (@=), buildProgram
                               , getUniform, useProgram )
import Photon.Render.GL.Texture as Tex ( Format(..), InternalFormat(..) )
import Photon.Render.Shader ( GPUProgram )
import Photon.Utils.Log

data Shadowing = Shadowing {
    _shadowDepthCubeOff        :: CubeOffscreen
  , _shadowShadowOff           :: Offscreen
  , _shadowCubeDepthmapProgram :: GPUProgram
  , _shadowShadowProgram       :: GPUProgram
  , _shadowUniforms            :: ShadowingUniforms
  }

data ShadowingUniforms = ShadowingUniforms {
    _shadowDepthLigProjViewsU :: Uniform [M44 Float]
  , _shadowDepthModelU        :: Uniform (M44 Float)
  , _shadowDepthLigPosU       :: Uniform (V3 Float)
  , _shadowDepthLigIRadU      :: Uniform Float
  , _shadowShadowLigPosU      :: Uniform (V3 Float)
  , _shadowShadowLigRadU      :: Uniform Float
  , _shadowShadowIProjViewU   :: Uniform (M44 Float)
  }

makeLenses ''Shadowing
makeLenses ''ShadowingUniforms

getShadowing :: (MonadIO m,MonadLogger m,MonadError Log m)
            => Natural
            -> Natural
            -> Natural
            -> m Shadowing
getShadowing w h cubeSize = do
  info CoreLog "generating light cube depthmap offscreen"
  cubeOff <- genCubeOffscreen cubeSize R32F Tex.R (ColorAttachment 0) Depth32F
    Depth DepthAttachment
  shadowOff <- genOffscreen w h RGB32F Tex.RGB
  depthProgram <- buildProgram shadowDepthCubemapVS (Just shadowDepthCubemapGS)
    shadowDepthCubemapFS
  shadowProgram <- buildProgram shadowShadowVS Nothing shadowShadowFS
  uniforms <- liftIO (getShadowingUniforms depthProgram shadowProgram)
  return (Shadowing cubeOff shadowOff depthProgram shadowProgram uniforms)

getShadowingUniforms :: GPUProgram -> GPUProgram -> IO ShadowingUniforms
getShadowingUniforms depthProgram shadowProgram = do
    useProgram shadowProgram
    shadowSem "depthmap" >>= (@= (0 :: Int))
    shadowSem "ligDepthmap" >>= (@= (1 :: Int))
    ShadowingUniforms
      <$> depthSem "ligProjViews"
      <*> depthSem "model"
      <*> depthSem "ligPos"
      <*> depthSem "ligIRad"
      <*> shadowSem "ligPos"
      <*> shadowSem "ligRad"
      <*> shadowSem "iProjView"
  where
    depthSem :: (Uniformable a) => String -> IO (Uniform a)
    depthSem = getUniform depthProgram
    shadowSem :: (Uniformable a) => String -> IO (Uniform a)
    shadowSem = getUniform shadowProgram

purgeShadowingFramebuffer :: Shadowing -> IO ()
purgeShadowingFramebuffer shadowing = do
  bindFramebuffer (shadowing^.shadowShadowOff.offscreenFB) ReadWrite
  glClearColor 0 0 0 0
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  bindFramebuffer (shadowing^.shadowDepthCubeOff.cubeOffscreenFB) ReadWrite
  glClearColor 1 1 1 1
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT

shadowDepthCubemapVS :: String
shadowDepthCubemapVS = unlines
  [
    "#version 330 core"

  , "layout (location = 0) in vec3 co;"
  , "layout (location = 1) in vec3 no;"

  , "uniform mat4 model;"

  , "void main() {"
  , "  gl_Position = model * vec4(co,1.);"
  , "}"
  ]

-- The geometry shader is used because we’re doing a layered rendering in order
-- to generate the whole cube depthmap in one pass. Each primitive (i.e.
-- triangle) gets duplicate 6 times; one time per cubemap face.
shadowDepthCubemapGS :: String
shadowDepthCubemapGS = unlines
  [
    "#version 330 core"

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
  , "      gl_Position = ligProjViews[i] * (gl_in[j].gl_Position - ligPos);"
  , "      EmitVertex();"
  , "    }"
  , "    EndPrimitive();"
  , "  }"
  , "}"
  ]

shadowDepthCubemapFS :: String
shadowDepthCubemapFS = unlines
  [
    "#version 330 core"

  , "in vec3 gco;"
  , "out float outDistance;"

  , "uniform vec3 ligPos;"
  , "uniform float ligIRad;"

  , "void main() {"
  , "  outDistance = distance(ligPos,gco) * ligIRad;"
  , "}"
  ]

shadowShadowVS :: String
shadowShadowVS = unlines
  [
    "#version 330 core"

  , "out vec2 vv;"

  , "vec2[4] v = vec2[]("
  , "    vec2(-1,  1)"
  , "  , vec2( 1,  1)"
  , "  , vec2(-1, -1)"
  , "  , vec2( 1, -1)"
  , "  );"

  , "void main() {"
  , "  vv = v[gl_VertexID];"
  , "  gl_Position = vec4(vv, 0., 1.);"
  , "}"
  ]

shadowShadowFS :: String
shadowShadowFS = unlines
  [
    "#version 330 core"

  , "in vec2 vv;"

  , "out vec3 shadow;"

  , "uniform vec3 ligPos;"
  , "uniform float ligRad;"
  , "uniform mat4 iProjView;"
  , "uniform sampler2D depthmap;"
  , "uniform samplerCube ligDepthmap;"

  , "vec3 deproject() {"
  , "  float depth = 2. * texelFetch(depthmap, ivec2(gl_FragCoord.xy), 0).r - 1.;"
  , "  vec4 position = vec4(vv, depth, 1.);"
  , "  position = iProjView * position;"
  , "  position.xyz /= position.w;"
  , "  return position.xyz;"
  , "}"

  , "void main() {"
  , "  vec3 co = deproject();"

  , "  float bias = 0.005;"
  , "  vec3 depthDir = co - ligPos;"
    -- the min ensures we don’t exceed the light radius
    -- TODO: this might generate artifacts near the light zfar
  , "  float distReceiver = min(ligRad,length(depthDir) - bias);"
  , "  float distBlocker = texture(ligDepthmap, depthDir).r;"

  , "  shadow = vec3(1.,1.,1.);"
  , "  if (distBlocker*ligRad < distReceiver) {"
  , "    shadow = vec3(0.,0.,0.);"
  , "  }"
  , "}"
  ]
