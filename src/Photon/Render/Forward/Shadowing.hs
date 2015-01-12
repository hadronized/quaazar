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
import Control.Lens ( makeLenses )
import Control.Monad.Trans ( MonadIO(..) )
import Control.Monad.Trans.Either ( EitherT, hoistEither )
import Control.Monad.Trans.Journal ( evalJournalT )
import Linear
import Numeric.Natural ( Natural )
import Photon.Core.Projection ( Projection(..), projectionMatrix )
import Photon.Render.GL.GLObject
import Photon.Render.GL.Framebuffer ( AttachmentPoint(..), Framebuffer
                                    , Target(..), attachTexture
                                    , buildFramebuffer )
import Photon.Render.GL.Shader ( Uniform, Uniformable, buildProgram
                               , getUniform )
import Photon.Render.GL.Texture as Tex ( Cubemap, Filter(..), Format(..)
                                       , InternalFormat(..), TextureLike(..)
                                       , Wrap(..) )
import Photon.Render.Shader ( GPUProgram )
import Photon.Utils.Log

data Shadowing = Shadowing {
    _shadowCubeDepthFB :: Framebuffer
  , _shadowCubeRender          :: Cubemap
  , _shadowCubeDepthmap        :: Cubemap
  , _shadowCubeDepthmapProgram :: GPUProgram
  , _shadowUniforms            :: ShadowingUniforms
  , _shadowProjection          :: M44 Float
  }

data ShadowingUniforms = ShadowingUniforms {
    _shadowLigProjViewsU :: Uniform [M44 Float]
  , _shadowModelU        :: Uniform (M44 Float)
  , _shadowLigPosU       :: Uniform (V3 Float)
  , _shadowLigIRadU      :: Uniform Float
  }

makeLenses ''Shadowing
makeLenses ''ShadowingUniforms

getShadowing :: Natural -> Float -> Float -> EitherT Log IO Shadowing
getShadowing cubeSize znear zfar = do
  liftIO . print $ Log InfoLog CoreLog "generating light cube depthmap offscreen"
  program <- evalJournalT $
    buildProgram lightCubeDepthmapVS (Just lightCubeDepthmapGS) lightCubeDepthmapFS <* sinkLogs
  uniforms <- liftIO (getShadowingUniforms program)
  (colormap,depthmap) <- liftIO $ do
    -- TODO: refactoring
    colormap <- genObject
    bindTexture colormap
    setTextureWrap colormap ClampToEdge
    setTextureFilters colormap Linear
    setTextureNoImage colormap R32F cubeSize cubeSize Tex.R
    unbindTexture colormap

    -- TODO: refactoring
    depthmap <- genObject
    bindTexture depthmap
    setTextureWrap depthmap ClampToEdge
    setTextureFilters depthmap Linear
    setTextureNoImage depthmap Depth32F cubeSize cubeSize Depth
    --setTextureCompareFunc depthmap (Just LessOrEqual)
    unbindTexture depthmap

    return (colormap,depthmap)

  fb' <- liftIO $ buildFramebuffer ReadWrite $ \_ -> do
    attachTexture ReadWrite colormap (ColorAttachment 0)
    attachTexture ReadWrite depthmap DepthAttachment
  fb <- hoistEither fb'
  return (Shadowing fb colormap depthmap program uniforms proj)
  where
    proj = projectionMatrix $ Perspective (pi/2) 1 znear zfar

getShadowingUniforms :: GPUProgram -> IO ShadowingUniforms
getShadowingUniforms program = do
    ShadowingUniforms
      <$> sem "ligProjViews"
      <*> sem "model"
      <*> sem "ligPos"
      <*> sem "ligIRad"
  where
    sem :: (Uniformable a) => String -> IO (Uniform a)
    sem = getUniform program

lightCubeDepthmapVS :: String
lightCubeDepthmapVS = unlines
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
lightCubeDepthmapGS :: String
lightCubeDepthmapGS = unlines
  [
    "#version 330 core"

  , "layout (triangles) in;"
  , "layout (triangle_strip, max_vertices = 18) out;"

  , "out vec3 gco;"

  , "uniform mat4 ligProjViews[6];" -- 6 views

  , "void main() {"
  , "  for (int i = 0; i < 6; ++i) {"
  , "    for (int j = 0; j < 3; ++j) {"
  , "      gl_Layer = i;"
  , "      gco = gl_in[j].gl_Position.xyz;"
  , "      gl_Position = ligProjViews[i] * gl_in[j].gl_Position;"
  , "      EmitVertex();"
  , "    }"
  , "    EndPrimitive();"
  , "  }"
  , "}"
  ]

lightCubeDepthmapFS :: String
lightCubeDepthmapFS = unlines
  [
    "#version 330 core"

  , "in vec3 gco;"
  , "out float outDistance;"

  , "uniform vec3 ligPos;"
  , "uniform float ligIRad;"

  , "void main() {"
  , "  outDistance = length(gco) * ligIRad;"
  , "}"
  ]
