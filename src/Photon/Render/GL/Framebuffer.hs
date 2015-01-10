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

import Control.Monad ( void )
import Foreign.Marshal ( alloca )
import Foreign.Marshal.Array ( peekArray, withArrayLen )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Photon.Render.GL.GLObject
import Photon.Render.GL.Log
import Photon.Render.GL.Renderbuffer ( Renderbuffer(..) )
import Photon.Render.GL.Texture ( TextureLike(textureID) )
import Photon.Utils.Log

newtype Framebuffer = Framebuffer { unFramebuffer :: GLuint } deriving (Eq,Show)

instance GLObject Framebuffer where
  genObjects n = alloca $ \p -> do
    glGenFramebuffers (fromIntegral n) p
    fmap (map Framebuffer) $ peekArray n p
  deleteObjects a = withArrayLen (map unFramebuffer a) $ \s p ->
    glDeleteFramebuffers (fromIntegral s) p

data Target
  = Read
  | Write
  | ReadWrite
    deriving (Eq,Show)

data AttachmentPoint
  = ColorAttachment Natural
  | DepthAttachment
    deriving (Eq,Show)

bindFramebuffer :: Framebuffer -> Target -> IO ()
bindFramebuffer (Framebuffer fb) target = glBindFramebuffer target' fb
  where
    target' = fromTarget target

unbindFramebuffer :: Target -> IO ()
unbindFramebuffer target = glBindFramebuffer target' 0
  where
    target' = fromTarget target

checkFramebufferStatus :: IO (Maybe String)
checkFramebufferStatus = fmap treatStatus (glCheckFramebufferStatus gl_FRAMEBUFFER)
  where
    treatStatus status
        | status == gl_FRAMEBUFFER_COMPLETE = Nothing
        | status == gl_FRAMEBUFFER_UNDEFINED = Just "default framebuffer"
        | status == gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = Just "at least one attachment point is wrong"
        | status == gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = Just "lacks at least one attached image"
        | status == gl_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER = Just "doesn't have any color attachment"
        | status == gl_FRAMEBUFFER_INCOMPLETE_READ_BUFFER = Just "read buffer"
        | status == gl_FRAMEBUFFER_UNSUPPORTED = Just "internal formats mismatch"
        | status == gl_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE = Just "incomplete multisample"
        | status == gl_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS = Just "layer targets"
        | otherwise = Just "unknown error"

attachTextureAt :: (TextureLike t)
                => Target
                -> t
                -> AttachmentPoint
                -> Natural
                -> IO ()
attachTextureAt target tex ap level = glFramebufferTexture target' ap' tid lvl
  where
    target' = fromTarget target
    ap' = fromAttachmentPoint ap
    tid = textureID tex
    lvl = fromIntegral level

attachTexture :: (TextureLike t) => Target -> t -> AttachmentPoint -> IO ()
attachTexture target tex ap = attachTextureAt target tex ap 0

attachRenderbuffer :: Target -> Renderbuffer -> AttachmentPoint -> IO ()
attachRenderbuffer target (Renderbuffer rbuf) ap = glFramebufferRenderbuffer target' ap' gl_RENDERBUFFER rbuf
  where
    target' = fromTarget target
    ap'     = fromAttachmentPoint ap

buildFramebuffer :: Target -> (Framebuffer -> IO a) -> IO (Either Log Framebuffer)
buildFramebuffer target f = do
    fb <- genObject
    bindFramebuffer fb target
    void (f fb)
    checkFramebufferStatus >>= return . maybe (Right fb) onError
  where
    onError = Left . Log ErrorLog gllog

drawBuffers :: [AttachmentPoint] -> IO ()
drawBuffers bufs = withArrayLen (map fromAttachmentPoint bufs) (\s b -> glDrawBuffers (fromIntegral s) b)

fromTarget :: Target -> GLenum
fromTarget target = case target of
  Read  -> gl_READ_FRAMEBUFFER
  Write -> gl_DRAW_FRAMEBUFFER
  ReadWrite -> gl_FRAMEBUFFER

fromAttachmentPoint :: AttachmentPoint -> GLenum
fromAttachmentPoint ap = case ap of
  ColorAttachment i -> gl_COLOR_ATTACHMENT0 + fromIntegral i
  DepthAttachment   -> gl_DEPTH_ATTACHMENT
