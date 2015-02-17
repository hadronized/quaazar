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

module Quaazar.Render.GL.Offscreen where

import Control.Lens ( makeLenses )
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Data.Either.Combinators ( eitherToError )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Render.GL.Framebuffer
import Quaazar.Render.GL.GLObject
import Quaazar.Render.GL.Texture
import Quaazar.Utils.Log

data Offscreen = Offscreen {
    _offscreenRender   :: Texture2D
  , _offscreenDepthmap :: Texture2D
  , _offscreenFB       :: Framebuffer
  } deriving (Eq)

makeLenses ''Offscreen

genOffscreen :: (MonadScoped IO m,MonadIO m,MonadError Log m)
             => Natural
             -> Natural
             -> Filter
             -> InternalFormat
             -> Format
             -> m Offscreen
genOffscreen w h flt texift texft = do
  (colormap,depthmap,fb') <- do
    colormap <- genObject
    bindTexture colormap
    setTextureWrap colormap ClampToEdge
    setTextureFilters colormap flt
    setTextureNoImage colormap texift w h texft
    unbindTexture colormap
    depthmap <- genObject
    bindTexture depthmap
    setTextureWrap depthmap ClampToEdge
    setTextureFilters depthmap flt
    setTextureNoImage depthmap Depth32F w h Depth
    unbindTexture depthmap
    fb <- buildFramebuffer ReadWrite . const $ do
      attachTexture ReadWrite colormap (ColorAttachment 0)
      attachTexture ReadWrite depthmap DepthAttachment
    return (colormap,depthmap,fb)
  fb <- eitherToError fb'
  return (Offscreen colormap depthmap fb)

data DepthOffscreen = DepthOffscreen {
    _depthOffscreenTex :: Texture2D
  , _depthOffscreenFB  :: Framebuffer
  }

makeLenses ''DepthOffscreen

genDepthOffscreen :: (MonadScoped IO m,MonadIO m,MonadError Log m)
                  => Natural
                  -> Natural
                  -> Filter
                  -> m DepthOffscreen
genDepthOffscreen w h flt = do
  tex <- genObject
  bindTexture tex
  setTextureWrap tex ClampToEdge
  setTextureFilters tex flt
  setTextureNoImage tex Depth32F w h Depth
  unbindTexture tex
  fb' <- buildFramebuffer ReadWrite . const $ do
    attachTexture ReadWrite tex DepthAttachment
    liftIO $ glDrawBuffer gl_NONE
  fb <- eitherToError fb'
  return (DepthOffscreen tex fb)

data CubeOffscreen = CubeOffscreen {
    _cubeOffscreenColorTex :: Cubemap
  , _cubeOffscreenDepthTex :: Cubemap
  , _cubeOffscreenFB       :: Framebuffer
  }

makeLenses ''CubeOffscreen

genCubeOffscreen :: (MonadScoped IO m,MonadIO m,MonadError Log m)
                 => Natural
                 -> Filter
                 -> InternalFormat
                 -> Format
                 -> AttachmentPoint
                 -> InternalFormat
                 -> Format
                 -> AttachmentPoint
                 -> m CubeOffscreen
genCubeOffscreen cubeSize flt colift colft colap depthift depthft depthap = do
  (colormap,depthmap,fb') <- do
    -- color cubemap
    colormap <- genObject
    bindTexture colormap
    setTextureWrap colormap ClampToEdge
    setTextureFilters colormap flt
    setTextureNoImage colormap colift cubeSize cubeSize colft
    unbindTexture colormap
    -- depth cubemap
    depthmap <- genObject
    bindTexture depthmap
    setTextureWrap depthmap ClampToEdge
    setTextureFilters depthmap flt
    setTextureNoImage depthmap depthift cubeSize cubeSize depthft
    --setTextureCompareFunc depthmap (Just LessOrEqual)
    unbindTexture depthmap
    -- framebuffer
    fb <- buildFramebuffer ReadWrite . const $ do
      attachTexture ReadWrite colormap colap
      attachTexture ReadWrite depthmap depthap
    return (colormap,depthmap,fb)
  fb <- eitherToError fb'
  return (CubeOffscreen colormap depthmap fb)