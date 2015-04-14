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
    setTextureStorage colormap texift w h
    unbindTexture colormap
    depthmap <- genObject
    bindTexture depthmap
    setTextureWrap depthmap ClampToEdge
    setTextureFilters depthmap flt
    setTextureStorage depthmap Depth32F w h
    unbindTexture depthmap
    fb <- buildFramebuffer ReadWrite . const $ do
      attachTexture ReadWrite colormap (ColorAttachment 0)
      attachTexture ReadWrite depthmap DepthAttachment
    return (colormap,depthmap,fb)
  fb <- eitherToError fb'
  return (Offscreen colormap depthmap fb)

data OffscreenArray = OffscreenArray {
    _offscreenArrayColormaps :: Texture2DArray
  , _offscreenArrayDepthmaps :: Texture2DArray
  , _offscreenArrayFB        :: Framebuffer
  }

makeLenses ''OffscreenArray

genOffscreenArray :: (MonadScoped IO m,MonadIO m,MonadError Log m)
                  => Natural
                  -> Natural
                  -> Natural
                  -> Filter
                  -> InternalFormat
                  -> Format
                  -> AttachmentPoint
                  -> InternalFormat
                  -> Format
                  -> AttachmentPoint
                  -> m OffscreenArray
genOffscreenArray w h n flt colift colft colap depthift depthft depthap = do
  (colormaps,depthmaps,fb') <- do
    -- colormaps
    colormaps <- genObject
    bindTexture colormaps
    setTextureWrap colormaps ClampToEdge
    setTextureFilters colormaps flt
    setTextureStorageLayer colormaps colift w h n
    unbindTexture colormaps
    -- depthmaps
    depthmaps <- genObject
    bindTexture depthmaps
    setTextureWrap depthmaps ClampToEdge
    setTextureFilters depthmaps flt
    setTextureStorageLayer depthmaps depthift w h n
    --setTextureCompareFunc depthmaps (Just LessOrEqual)
    unbindTexture depthmaps
    -- framebuffer
    fb <- buildFramebuffer ReadWrite . const $ do
      attachTexture ReadWrite colormaps colap
      attachTexture ReadWrite depthmaps depthap
    return (colormaps,depthmaps,fb)
  fb <- eitherToError fb'
  return (OffscreenArray colormaps depthmaps fb)

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
  setTextureStorage tex Depth32F w h
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
    setTextureStorage colormap colift cubeSize cubeSize
    unbindTexture colormap
    -- depth cubemap
    depthmap <- genObject
    bindTexture depthmap
    setTextureWrap depthmap ClampToEdge
    setTextureFilters depthmap flt
    setTextureStorage depthmap depthift cubeSize cubeSize
    --setTextureCompareFunc depthmap (Just LessOrEqual)
    unbindTexture depthmap
    -- framebuffer
    fb <- buildFramebuffer ReadWrite . const $ do
      attachTexture ReadWrite colormap colap
      attachTexture ReadWrite depthmap depthap
    return (colormap,depthmap,fb)
  fb <- eitherToError fb'
  return (CubeOffscreen colormap depthmap fb)

data CubeOffscreenArray = CubeOffscreenArray {
    _cubeOffscreenArrayColormaps :: CubemapArray
  , _cubeOffscreenArrayDepthmaps :: CubemapArray
  , _cubeOffscreenArrayFB        :: Framebuffer
  }

makeLenses ''CubeOffscreenArray

genCubeOffscreenArray :: (MonadIO m,MonadScoped IO m,MonadError Log m)
                      => Natural
                      -> Natural
                      -> Filter
                      -> InternalFormat
                      -> Format
                      -> AttachmentPoint
                      -> InternalFormat
                      -> Format
                      -> AttachmentPoint
                      -> m CubeOffscreenArray
genCubeOffscreenArray cubeSize n flt colift colft colap depthift depthft depthap = do
  (colormaps,depthmaps,fb') <- do
    -- color cubemaps
    colormaps <- genObject
    bindTexture colormaps
    setTextureWrap colormaps ClampToEdge
    setTextureFilters colormaps flt
    setTextureStorageLayer colormaps colift cubeSize cubeSize n
    unbindTexture colormaps
    -- depth cubemaps
    depthmaps <- genObject
    bindTexture depthmaps
    setTextureWrap depthmaps ClampToEdge
    setTextureFilters depthmaps flt
    setTextureStorageLayer depthmaps depthift cubeSize cubeSize n
    --setTextureCompareFunc depthmaps (Just LessOrEqual)
    unbindTexture depthmaps
    -- framebuffer
    fb <- buildFramebuffer ReadWrite . const $ do
      attachTexture ReadWrite colormaps colap
      attachTexture ReadWrite depthmaps depthap
    return (colormaps,depthmaps,fb)
  fb <- eitherToError fb'
  return (CubeOffscreenArray colormaps depthmaps fb)
