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

module Quaazar.Render.GL.Framebuffer where

import Control.Monad ( void )
import Control.Monad.Trans ( MonadIO(..) )
import Foreign.Marshal.Array ( withArrayLen )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Render.GL.GLObject
import Quaazar.Render.GL.Log
import Quaazar.Render.GL.Renderbuffer ( Renderbuffer(..) )
import Quaazar.Render.GL.Texture ( IsTexture(textureID) )
import Quaazar.Utils.Log

newtype Framebuffer = Framebuffer { unFramebuffer :: GLuint } deriving (Eq,Show)

instance GLObject Framebuffer where
  genObjects n = genericGenObjects n glGenFramebuffers glDeleteFramebuffers Framebuffer

data Target
  = Read
  | Write
  | ReadWrite
    deriving (Eq,Show)

data AttachmentPoint
  = ColorAttachment Natural
  | DepthAttachment
    deriving (Eq,Show)

bindFramebuffer :: (MonadIO m) => Framebuffer -> Target -> m ()
bindFramebuffer (Framebuffer fb) target = liftIO $ glBindFramebuffer target' fb
  where
    target' = fromTarget target

unbindFramebuffer :: (MonadIO m) => Target -> m ()
unbindFramebuffer target = liftIO $ glBindFramebuffer target' 0
  where
    target' = fromTarget target

checkFramebufferStatus :: (MonadIO m) => m (Maybe String)
checkFramebufferStatus = liftIO $ fmap treatStatus (glCheckFramebufferStatus gl_FRAMEBUFFER)
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

attachTextureAt :: (MonadIO m,IsTexture t)
                => Target
                -> t
                -> AttachmentPoint
                -> Natural
                -> m ()
attachTextureAt target tex ap level = liftIO $ glFramebufferTexture target' ap' tid lvl
  where
    target' = fromTarget target
    ap' = fromAttachmentPoint ap
    tid = textureID tex
    lvl = fromIntegral level

attachTexture :: (MonadIO m,IsTexture t) => Target -> t -> AttachmentPoint -> m ()
attachTexture target tex ap = attachTextureAt target tex ap 0

attachRenderbuffer :: (MonadIO m) => Target -> Renderbuffer -> AttachmentPoint -> m ()
attachRenderbuffer target (Renderbuffer rbuf) ap = liftIO $ glFramebufferRenderbuffer target' ap' gl_RENDERBUFFER rbuf
  where
    target' = fromTarget target
    ap'     = fromAttachmentPoint ap

buildFramebuffer :: (MonadScoped IO m,MonadIO m) => Target -> (Framebuffer -> m a) -> m (Either Log Framebuffer)
buildFramebuffer target f = do
    fb <- genObject
    bindFramebuffer fb target
    void (f fb)
    checkFramebufferStatus >>= return . maybe (Right fb) onError
  where
    onError = Left . Log ErrorLog gllog

drawBuffers :: (MonadIO m) => [AttachmentPoint] -> m ()
drawBuffers bufs = liftIO $ withArrayLen (map fromAttachmentPoint bufs) (\s b -> glDrawBuffers (fromIntegral s) b)

fromTarget :: Target -> GLenum
fromTarget target = case target of
  Read  -> gl_READ_FRAMEBUFFER
  Write -> gl_DRAW_FRAMEBUFFER
  ReadWrite -> gl_FRAMEBUFFER

fromAttachmentPoint :: AttachmentPoint -> GLenum
fromAttachmentPoint ap = case ap of
  ColorAttachment i -> gl_COLOR_ATTACHMENT0 + fromIntegral i
  DepthAttachment   -> gl_DEPTH_ATTACHMENT
