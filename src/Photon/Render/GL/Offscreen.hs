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
import Numeric.Natural ( Natural )
import Photon.Render.GL.Framebuffer
import Photon.Render.GL.Log ( gllog )
import Photon.Render.GL.GLObject
import Photon.Render.GL.Renderbuffer
import Photon.Render.GL.Texture
import Photon.Utils.Log

-- |OpenGL requires three objects to perform an offscreen render:
--
--   - a texture to capture the offscreen render ;
--   - a framebuffer to support the render ;
--   - a renderbuffer for… I have no fucking idea.
--
-- The 'Offscreen' type gathers those three objects and expose a simple
-- interface to deal with offscreen renders.
data Offscreen = Offscreen {
    _offscreenTex :: Texture2D
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

  fb <- genObject
  bindFramebuffer fb Write
  attachTexture Write tex texap
  attachRenderbuffer Write rb rbap

  status <- checkFramebufferStatus
  unbindFramebuffer Write

  maybe (return . Right $ Offscreen tex fb rb) (return . Left . Log ErrorLog gllog) status
