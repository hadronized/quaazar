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

module Photon.Render.GL.Buffer where

import Control.Applicative
import Foreign.Concurrent
import Foreign.Marshal ( malloc, free )
import Foreign.Marshal.Array ( withArray )
import Foreign.Ptr ( nullPtr )
import Foreign.Storable ( Storable )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Photon.Render.GL.GLObject

newtype Buffer = Buffer { unBuffer :: GLObject } deriving (Eq,Show)

data BufferTarget
  = ArrayBuffer
  | IndexBuffer
    deriving (Eq,Show)

genBuffer :: IO Buffer
genBuffer = do
  p <- malloc
  glGenBuffers 1 p
  Buffer . GLObject <$> newForeignPtr p (glDeleteBuffers 1 p >> free p)

bindBuffer :: Buffer -> BufferTarget -> IO ()
bindBuffer (Buffer buffer) target = withGLObject buffer (glBindBuffer target')
  where
    target' = fromBufferTarget target

unbindBuffer :: BufferTarget -> IO ()
unbindBuffer target = glBindBuffer target' 0
  where
    target' = fromBufferTarget target

initBuffer :: BufferTarget -> Natural -> IO ()
initBuffer target bytes = glBufferData target' (fromIntegral bytes) nullPtr gl_STATIC_DRAW -- FIXME
  where
    target' = fromBufferTarget target

bufferSubData :: (Storable a) => BufferTarget -> Int -> Natural -> [a] -> IO ()
bufferSubData target offset bytes values =
    withArray values $ glBufferSubData target' (fromIntegral offset) (fromIntegral bytes)
  where
    target' = fromBufferTarget target

fromBufferTarget :: BufferTarget -> GLenum
fromBufferTarget target = case target of
  ArrayBuffer   -> gl_ARRAY_BUFFER
  IndexBuffer -> gl_ELEMENT_ARRAY_BUFFER
