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

module Photon.Render.GL.Offscreen where

import Control.Lens ( makeLenses )
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Photon.Render.GL.Framebuffer
import Photon.Render.GL.Log ( gllog )
import Photon.Render.GL.GLObject
import Photon.Render.GL.Renderbuffer
import Photon.Render.GL.Texture
import Photon.Utils.Either ( generalizeEither )
import Photon.Utils.Log

data Offscreen = Offscreen {
    _offscreenTex :: Texture2D
  , _offscreenFB  :: Framebuffer
  , _offscreenRB  :: Renderbuffer
  } deriving (Eq)

makeLenses ''Offscreen

genOffscreen :: (MonadIO m,MonadError Log m)
             => Natural
             -> Natural
             -> InternalFormat
             -> Format
             -> AttachmentPoint
             -> InternalFormat
             -> AttachmentPoint
             -> m Offscreen
genOffscreen w h texift texft texap rbift rbap = do
  (tex,rb,fb') <- liftIO $ do
    tex <- genObject
    bindTexture tex
    setTextureWrap tex ClampToEdge
    setTextureFilters tex Nearest
    setTextureNoImage tex texift w h texft
    unbindTexture tex
    rb <- genObject
    bindRenderbuffer rb
    renderbufferStorage rbift w h
    unbindRenderbuffer
    fb <- buildFramebuffer ReadWrite . const $ do
      attachTexture ReadWrite tex texap
      attachRenderbuffer ReadWrite rb rbap
    return (tex,rb,fb)
  fb <- generalizeEither fb'
  return (Offscreen tex fb rb)

data DepthOffscreen = DepthOffscreen {
    _depthOffscreenTex :: Texture2D
  , _depthOffscreenFB  :: Framebuffer
  }

makeLenses ''DepthOffscreen

genDepthOffscreen :: (MonadIO m,MonadError Log m)
                  => Natural
                  -> Natural
                  -> m DepthOffscreen
genDepthOffscreen w h = do
  (tex,fb') <- liftIO $ do
    tex <- genObject
    bindTexture tex
    setTextureWrap tex ClampToEdge
    setTextureFilters tex Nearest
    setTextureNoImage tex Depth32F w h Depth
    unbindTexture tex
    fb <- buildFramebuffer ReadWrite . const $ do
      attachTexture ReadWrite tex DepthAttachment
      glDrawBuffer gl_NONE
    return (tex,fb)
  fb <- generalizeEither fb'
  return (DepthOffscreen tex fb)

data CubeOffscreen = CubeOffscreen {
    _cubeOffscreenColorTex :: Cubemap
  , _cubeOffscreenDepthTex :: Cubemap
  , _cubeOffscreenFB       :: Framebuffer
  }

makeLenses ''CubeOffscreen

genCubeOffscreen :: (MonadIO m,MonadError Log m)
                 => Natural
                 -> InternalFormat
                 -> Format
                 -> AttachmentPoint
                 -> InternalFormat
                 -> Format
                 -> AttachmentPoint
                 -> m CubeOffscreen
genCubeOffscreen cubeSize colift colft colap depthift depthft depthap = do
  (colormap,depthmap,fb') <- liftIO $ do
    -- color cubemap
    colormap <- genObject
    bindTexture colormap
    setTextureWrap colormap ClampToEdge
    setTextureFilters colormap Linear
    setTextureNoImage colormap colift cubeSize cubeSize colft
    unbindTexture colormap
    -- depth cubemap
    depthmap <- genObject
    bindTexture depthmap
    setTextureWrap depthmap ClampToEdge
    setTextureFilters depthmap Linear
    setTextureNoImage depthmap depthift cubeSize cubeSize depthft
    --setTextureCompareFunc depthmap (Just LessOrEqual)
    unbindTexture depthmap
    -- framebuffer
    fb <- buildFramebuffer ReadWrite . const $ do
      attachTexture ReadWrite colormap colap
      attachTexture ReadWrite depthmap depthap
    return (colormap,depthmap,fb)
  fb <- generalizeEither fb'
  return (CubeOffscreen colormap depthmap fb)
