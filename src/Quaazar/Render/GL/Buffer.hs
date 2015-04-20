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

-- |An OpenGL buffer is a memory region reserved for any kind of purposes.
-- It can be used to store vertices’ components, or to pass values to a
-- shader’s computation.
newtype Buffer = Buffer { unBuffer :: GLuint } deriving (Eq,Ord,Show)

instance GLObject Buffer where
  genObjects n = genericGenObjects n glGenBuffers glDeleteBuffers Buffer

-- |Buffer target.
--
-- 'ArrayBuffer' is used to pack vertices’ components, like positions,
-- normals and UV textures.
--
-- 'IndexBuffer' stores all vertices indices used to create surfaces or
-- connect vertices between each other. You should fill that kind of
-- buffer with nothing but 'Word32' values.
--
-- 'ShaderStorageBuffer' is a buffer that contain arbitrary data that
-- can be referenced from a shader’s computation.
data Target
  = ArrayBuffer
  | IndexBuffer
  | ShaderStorageBuffer
    deriving (Eq,Show)

-- |When mapping a buffer into host’s space memory, the type of operation
-- that will be performed on the region must be supplied.
--
-- 'Read' allows read-only operations on the buffer.
--
-- 'Write' allows only write-only operations on the buffer.
--
-- 'ReadWrite' allows both operations on the buffer.
data MapAccess
  = Read
  | Write
  | ReadWrite
    deriving (Eq,Show)

-- |Bind a 'Buffer' to a 'Target' into the current OpenGL context.
bindBuffer :: (MonadIO m) => Buffer -> Target -> m ()
bindBuffer (Buffer buffer) target = liftIO $
  glBindBuffer (fromTarget target) buffer

-- |For indexed target only, bind a 'Buffer' to a 'Target' at a given
-- index into the current OpenGL context.
bindBufferAt :: (MonadIO m) => Buffer -> Target -> Natural -> m ()
bindBufferAt (Buffer buffer) target index = liftIO $
  glBindBufferBase (fromTarget target) (fromIntegral index) buffer

-- |Unbind the buffer from a 'Target' into the OpenGL context.
unbindBuffer :: (MonadIO m) => Target -> m ()
unbindBuffer target = liftIO $
  glBindBuffer (fromTarget target) 0

-- TODO: rename that to be coherent with texture storage allocation
-- |Reserve some storage for the currently bound buffer. The 'bytes'
-- argument is the total allocation size requested, in bytes.
initBuffer :: (MonadIO m) => Target -> Natural -> m ()
initBuffer target bytes = liftIO $
    glBufferData target' (fromIntegral bytes) nullPtr gl_STATIC_DRAW -- FIXME
  where
    target' = fromTarget target

-- TODO: rename that to be coherent with texture texels transfer
-- TODO: I think we can remove 'bytes' and use 'withArrayLen' and 'sizeOf'
-- |Fill the currently bound buffer with @(Storable a) => [a]@ data.
--
-- 'offset' is the offset to poke values to in the buffer. Set to '0' to poke
-- at the beginning of the buffer.
--
-- 'bytes' is the total bytes of data to transfer
bufferSubData :: (MonadIO m,Storable a) => Target -> Int -> Natural -> [a] -> m ()
bufferSubData target offset bytes values = liftIO $
    withArray values $ glBufferSubData target' (fromIntegral offset) (fromIntegral bytes)
  where
    target' = fromTarget target

-- |Map the currently bound buffer into the host’s memory space.
mapBuffer :: (MonadIO m) => Target -> MapAccess -> m (Ptr Word8)
mapBuffer target access = liftIO $ glMapBuffer (fromTarget target) (fromMapAccess access)

-- |Unmap the currently bound buffer into the current OpenGL context.
unmapBuffer :: (MonadIO m) => Target -> m Bool
unmapBuffer = liftIO . fmap toBool . glUnmapBuffer . fromTarget

-- |Map and unmap a buffer at the same type. Provides a function to use the mapped
-- buffer.
withMappedBuffer :: (MonadIO m) => Target -> MapAccess -> (Ptr Word8 -> m a) -> m Bool
withMappedBuffer target access f =
  mapBuffer target access >>= f >> unmapBuffer target

-- |Turns a 'Target' into its OpenGL 'GLenum' equivalent.
fromTarget :: Target -> GLenum
fromTarget target = case target of
  ArrayBuffer         -> gl_ARRAY_BUFFER
  IndexBuffer         -> gl_ELEMENT_ARRAY_BUFFER
  ShaderStorageBuffer -> gl_SHADER_STORAGE_BUFFER

-- |Turns a 'MapAccess' into its OpenGL 'GLenum' equivalent.
fromMapAccess :: MapAccess -> GLenum
fromMapAccess ma = case ma of
  Read -> gl_READ_ONLY
  Write -> gl_WRITE_ONLY
  ReadWrite -> gl_READ_WRITE
