-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Framebuffers are OpenGL objects used as render slots. They can have
-- attachments that dispatch render’s outputs to textures, renderbuffers
-- and so on and so forth.
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

-- |OpenGL framebuffer.
newtype Framebuffer = Framebuffer { unFramebuffer :: GLuint } deriving (Eq,Show)

instance GLObject Framebuffer where
  genObjects n = genericGenObjects n glGenFramebuffers glDeleteFramebuffers Framebuffer

-- |Framebuffer target.
--
-- 'Read' framebuffer allows read-only computations.
--
-- 'Write' framebuffer allows write-only computations.
--
-- 'ReadWrite' framebuffer allows both type of computations.
data Target
  = Read
  | Write
  | ReadWrite
    deriving (Eq,Show)

-- |An 'AttachmentPoint' is used when attaching a texture or a renderbuffer.
-- An attachment point references a part of the framebuffer to capture. It can
-- be one of two things:
--
--   - @ColorAttachment i@: captures the color which output number is /i/
--   - @DepthAttachment@: captures the depth
data AttachmentPoint
  = ColorAttachment Natural
  | DepthAttachment
    deriving (Eq,Show)

-- |Bind a 'Framebuffer' to a 'Target' into the current OpenGL context.
bindFramebuffer :: (MonadIO m) => Framebuffer -> Target -> m ()
bindFramebuffer (Framebuffer fb) target = liftIO $ glBindFramebuffer target' fb
  where
    target' = fromTarget target

-- |Unbind the currently bound 'Framebuffer' from a 'Target' into the current
-- OpenGL context.
unbindFramebuffer :: (MonadIO m) => Target -> m ()
unbindFramebuffer target = liftIO $ glBindFramebuffer target' 0
  where
    target' = fromTarget target

-- |Check the currently bound 'Framebuffer' status. Returns 'Nothing' if the
-- framebuffer is complete; otherwise, @Just str@ where 'str' describes the
-- framebuffer error.
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

-- |Attach a level of a texture to the currently bound 'Framebuffer' at a given
-- 'AttachmentPoint'.
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

-- |Attach the base level of a texture to the currently bound 'Framebuffer' at a given
-- 'AttachmentPoint'.
attachTexture :: (MonadIO m,IsTexture t) => Target -> t -> AttachmentPoint -> m ()
attachTexture target tex ap = attachTextureAt target tex ap 0

-- |Attach a 'Renderbuffer' to the currently bound 'Framebuffer' at a given
-- 'AttachmentPoint'.
attachRenderbuffer :: (MonadIO m) => Target -> Renderbuffer -> AttachmentPoint -> m ()
attachRenderbuffer target (Renderbuffer rbuf) ap = liftIO $ glFramebufferRenderbuffer target' ap' gl_RENDERBUFFER rbuf
  where
    target' = fromTarget target
    ap'     = fromAttachmentPoint ap

-- |'buildFramebuffer target f' builds a brand new 'Framebuffer' by binding it to 'target'
-- and use the provided 'f' function to customize it. 'f' can be used to attach textures.
--
-- After 'f' has returned, the framebuffer is checked for errors. If errors were found,
-- they’re returned as a 'Log'. Otherwise, the 'Framebuffer' is returned.
buildFramebuffer :: (MonadScoped IO m,MonadIO m) => Target -> (Framebuffer -> m a) -> m (Either Log Framebuffer)
buildFramebuffer target f = do
    fb <- genObject
    bindFramebuffer fb target
    void (f fb)
    checkFramebufferStatus >>= return . maybe (Right fb) onError
  where
    onError = Left . Log ErrorLog gllog

-- |Set the draw buffers for the currently bound 'Framebuffer'.
drawBuffers :: (MonadIO m) => [AttachmentPoint] -> m ()
drawBuffers bufs = liftIO $ withArrayLen (map fromAttachmentPoint bufs) (\s b -> glDrawBuffers (fromIntegral s) b)

-- |Turns a 'Target' into its OpenGL 'GLenum' equivalent.
fromTarget :: Target -> GLenum
fromTarget target = case target of
  Read  -> gl_READ_FRAMEBUFFER
  Write -> gl_DRAW_FRAMEBUFFER
  ReadWrite -> gl_FRAMEBUFFER

-- |Turns a 'AttachmentPoint' into its OpenGL 'GLenum' equivalent.
fromAttachmentPoint :: AttachmentPoint -> GLenum
fromAttachmentPoint ap = case ap of
  ColorAttachment i -> gl_COLOR_ATTACHMENT0 + fromIntegral i
  DepthAttachment   -> gl_DEPTH_ATTACHMENT
