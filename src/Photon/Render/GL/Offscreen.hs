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
import Control.Monad ( forM_ )
import Numeric.Natural ( Natural )
import Photon.Render.GL.Framebuffer
import Photon.Render.GL.Renderbuffer
import Photon.Render.GL.Texture

-- |OpenGL requires three objects to perform an offscreen render:
--
--   - a texture to capture the offscreen render ;
--   - a framebuffer to support the render ;
--   - a renderbuffer for… I have no fucking idea.
--
-- The 'Offscreen' type gathers those three objects and expose a simple
-- interface to deal with offscreen renders.
data Offscreen = Offscreen {
    _offscreenTex :: Texture
  , _offscreenFB  :: Framebuffer
  , _offscreenRB  :: Renderbuffer
  } deriving (Eq)

makeLenses ''Offscreen

genOffscreen :: Natural
             -> Natural
             -> InternalFormat
             -> Format
             -> AttachmentPoint
             -> InternalFormat
             -> AttachmentPoint
             -> IO (Either Log Offscreen)
genOffscreen w h texift texft texap rbift rbap = do
  tex <- genTexture2D
  bindTexture tex
  setTextureWrap tex Clamp
  setTextureFilters tex Nearest
  setTextureNoImage tex texift w h texft
  unbindTexture tex

  rb <- genRenderbuffer
  bindRenderbuffer rb
  renderbufferStorage rbift w h
  unbindRenderbuffer

  fb <- genFramebuffer
  bindFramebuffer fb Write
  attachTexture Write tex texap
  attachRenderbuffer Write rb rbap

  status <- checkFramebufferStatus
  unbindFramebuffer Write

  maybe (return . Right $ Offscreen tex fb rb) (return . Left . Log ErrorLog gllog) status

genCubeOffscreen :: Natural
                 -> Natural
                 -> InternalFormat
                 -> Format
                 -> InternalFormat
                 -> IO (Either Log Offscreen)
genCubeOffscreen w h texift texft rbift = do
  cube <- genCubemap
  bindTexture cube
  setTextureWrap cube Clamp
  setTextureFilters cube Nearest
  setTextureNoImage cube texift w h texft
  unbindTexture cube

  rb <- genRenderbuffer
  bindRenderbuffer rb
  renderbufferStorage rbift w h
  unbindRenderbuffer

  fb <- genFramebuffer
  bindFramebuffer fb Write
  forM_ [0..5] (attachTextureLayer Write cube DepthAttachment)
  attachRenderbuffer Write rb (ColorAttachment 0)

  status <- checkFramebufferStatus
  unbindFramebuffer Write

  maybe (return . Right $ Offscreen cube fb rb) (return . Left . Log ErrorLog gllog) status