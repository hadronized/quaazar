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

module Photon.Render.GL.Framebuffer where

import Control.Applicative
import Foreign.Concurrent
import Foreign.Marshal ( malloc, free )
import Foreign.Marshal.Array ( withArrayLen )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Photon.Render.GL.GLObject
import Photon.Render.GL.Renderbuffer ( Renderbuffer(..) )
import Photon.Render.GL.Texture ( Texture(..), unTexture )

newtype Framebuffer = Framebuffer { unFramebuffer :: GLObject } deriving (Eq,Show)

data Target
  = Read
  | Write
    deriving (Eq,Show)

data AttachmentPoint
  = ColorAttachment Natural
  | DepthAttachment
    deriving (Eq,Show)

genFramebuffer :: IO Framebuffer
genFramebuffer = do
  p <- malloc
  glGenFramebuffers 1 p
  Framebuffer . GLObject <$> newForeignPtr p (glDeleteFramebuffers 1 p >> free p)

bindFramebuffer :: Framebuffer -> Target -> IO ()
bindFramebuffer (Framebuffer fb) target = withGLObject fb (glBindFramebuffer target')
  where
    target' = fromTarget target

unbindFramebuffer :: Target -> IO ()
unbindFramebuffer target = glBindFramebuffer target' 0
  where
    target' = fromTarget target

checkFramebufferStatus :: IO (Maybe String)
checkFramebufferStatus = fmap treatStatus (glCheckFramebufferStatus gl_DRAW_FRAMEBUFFER)
  where
    treatStatus status
        | status == gl_FRAMEBUFFER_COMPLETE = Nothing
        | status == gl_FRAMEBUFFER_UNDEFINED = Just "default framebuffer"
        | status == gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = Just "at least one attachment point is wrong"
        | status == gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = Just "lacks at least one attached image"
        | status == gl_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER = Just "doesn't have any color attachment"
        | status == gl_FRAMEBUFFER_INCOMPLETE_READ_BUFFER = Just "read buffer"
        | status == gl_FRAMEBUFFER_UNSUPPORTED = Just "internal formats mismatch"
        | otherwise = Just "unknown error"

attachTextureAt :: Target -> Texture -> AttachmentPoint -> Natural -> IO ()
attachTextureAt target tex ap level =
    withGLObject (unTexture tex) (\tex' -> glFramebufferTexture target' ap' tex' lvl)
  where
    target' = fromTarget target
    ap' = fromAttachmentPoint ap
    lvl = fromIntegral level

attachTexture :: Target -> Texture -> AttachmentPoint -> IO ()
attachTexture target tex ap = attachTextureAt target tex ap 0

attachTextureLayerAt :: Target -> Texture -> AttachmentPoint -> Natural -> Natural -> IO ()
attachTextureLayerAt target tex ap level layer =
    withGLObject (unTexture tex) (\tex' -> glFramebufferTextureLayer target' ap' tex' lvl lyr)
  where
    target' = fromTarget target
    ap' = fromAttachmentPoint ap
    lvl = fromIntegral level
    lyr = fromIntegral layer

attachTextureLayer :: Target -> Texture -> AttachmentPoint -> Natural -> IO ()
attachTextureLayer target tex ap = attachTextureLayerAt target tex ap 0

attachRenderbuffer :: Target -> Renderbuffer -> AttachmentPoint -> IO ()
attachRenderbuffer target rbuf ap =
    withGLObject (unRenderbuffer rbuf) (glFramebufferRenderbuffer target' ap' gl_RENDERBUFFER)
  where
    target' = fromTarget target
    ap'     = fromAttachmentPoint ap

drawBuffers :: [Natural] -> IO ()
drawBuffers bufs = withArrayLen (map fromIntegral bufs) (\s b -> glDrawBuffers (fromIntegral s) b)

fromTarget :: Target -> GLenum
fromTarget target = case target of
  Read  -> gl_READ_FRAMEBUFFER
  Write -> gl_DRAW_FRAMEBUFFER

fromAttachmentPoint :: AttachmentPoint -> GLenum
fromAttachmentPoint ap = case ap of
  ColorAttachment i -> gl_COLOR_ATTACHMENT0 + fromIntegral i
  DepthAttachment   -> gl_DEPTH_ATTACHMENT
