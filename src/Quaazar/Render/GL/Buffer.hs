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

module Quaazar.Render.GL.Buffer where

import Data.Word ( Word8 )
import Foreign
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Render.GL.GLObject

newtype Buffer = Buffer { unBuffer :: GLuint } deriving (Eq,Ord,Show)

instance GLObject Buffer where
  genObjects n = alloca $ \p -> do
    glGenBuffers (fromIntegral n) p
    fmap (map Buffer) $ peekArray n p
  deleteObjects a = withArrayLen (map unBuffer a) $ \s p ->
    glDeleteBuffers (fromIntegral s) p

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

bindBuffer :: Buffer -> Target -> IO ()
bindBuffer (Buffer buffer) target =
  glBindBuffer (fromTarget target) buffer

bindBufferAt :: Buffer -> Target -> Natural -> IO ()
bindBufferAt (Buffer buffer) target index =
  glBindBufferBase (fromTarget target) (fromIntegral index) buffer

unbindBuffer :: Target -> IO ()
unbindBuffer target =
  glBindBuffer (fromTarget target) 0

initBuffer :: Target -> Natural -> IO ()
initBuffer target bytes =
    glBufferData target' (fromIntegral bytes) nullPtr gl_STATIC_DRAW -- FIXME
  where
    target' = fromTarget target

bufferSubData :: (Storable a) => Target -> Int -> Natural -> [a] -> IO ()
bufferSubData target offset bytes values =
    withArray values $ glBufferSubData target' (fromIntegral offset) (fromIntegral bytes)
  where
    target' = fromTarget target

mapBuffer :: Target -> MapAccess -> IO (Ptr Word8)
mapBuffer target access = glMapBuffer (fromTarget target) (fromMapAccess access)

unmapBuffer :: Target -> IO Bool
unmapBuffer = fmap toBool . glUnmapBuffer . fromTarget

withMappedBuffer :: Target -> MapAccess -> (Ptr Word8 -> IO ()) -> IO Bool
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
