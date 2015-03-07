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

module Quaazar.Render.GL.Buffer where

import Control.Monad.Trans ( MonadIO(..) )
import Foreign
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Render.GL.GLObject

newtype Buffer = Buffer { unBuffer :: GLuint } deriving (Eq,Ord,Show)

instance GLObject Buffer where
  genObjects n = genericGenObjects n glGenBuffers glDeleteBuffers Buffer

data Target
  = ArrayBuffer
  | IndexBuffer
  | ShaderStorageBuffer
    deriving (Eq,Show)

data MapAccess
  = Read
  | Write
  | ReadWrite
    deriving (Eq,Show)

bindBuffer :: (MonadIO m) => Buffer -> Target -> m ()
bindBuffer (Buffer buffer) target = liftIO $
  glBindBuffer (fromTarget target) buffer

bindBufferAt :: (MonadIO m) => Buffer -> Target -> Natural -> m ()
bindBufferAt (Buffer buffer) target index = liftIO $
  glBindBufferBase (fromTarget target) (fromIntegral index) buffer

unbindBuffer :: (MonadIO m) => Target -> m ()
unbindBuffer target = liftIO $
  glBindBuffer (fromTarget target) 0

initBuffer :: (MonadIO m) => Target -> Natural -> m ()
initBuffer target bytes = liftIO $
    glBufferData target' (fromIntegral bytes) nullPtr gl_STATIC_DRAW -- FIXME
  where
    target' = fromTarget target

bufferSubData :: (MonadIO m,Storable a) => Target -> Int -> Natural -> [a] -> m ()
bufferSubData target offset bytes values = liftIO $
    withArray values $ glBufferSubData target' (fromIntegral offset) (fromIntegral bytes)
  where
    target' = fromTarget target

mapBuffer :: (MonadIO m) => Target -> MapAccess -> m (Ptr Word8)
mapBuffer target access = liftIO $ glMapBuffer (fromTarget target) (fromMapAccess access)

unmapBuffer :: (MonadIO m) => Target -> m Bool
unmapBuffer = liftIO . fmap toBool . glUnmapBuffer . fromTarget

withMappedBuffer :: (MonadIO m) => Target -> MapAccess -> (Ptr Word8 -> m ()) -> m Bool
withMappedBuffer target access f =
  mapBuffer target access >>= f >> unmapBuffer target

fromTarget :: Target -> GLenum
fromTarget target = case target of
  ArrayBuffer         -> gl_ARRAY_BUFFER
  IndexBuffer         -> gl_ELEMENT_ARRAY_BUFFER
  ShaderStorageBuffer -> gl_SHADER_STORAGE_BUFFER

fromMapAccess :: MapAccess -> GLenum
fromMapAccess ma = case ma of
  Read -> gl_READ_ONLY
  Write -> gl_WRITE_ONLY
  ReadWrite -> gl_READ_WRITE
