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

import Foreign.Marshal ( alloca )
import Foreign.Marshal.Array ( peekArray, withArray, withArrayLen )
import Foreign.Ptr ( nullPtr )
import Foreign.Storable ( Storable )
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
    deriving (Eq,Show)

bindBuffer :: Buffer -> Target -> IO ()
bindBuffer (Buffer buffer) target =
  glBindBuffer (fromBufferTarget target) buffer

unbindBuffer :: Target -> IO ()
unbindBuffer target =
  glBindBuffer (fromBufferTarget target) 0

initBuffer :: Target -> Natural -> IO ()
initBuffer target bytes =
    glBufferData target' (fromIntegral bytes) nullPtr gl_STATIC_DRAW -- FIXME
  where
    target' = fromBufferTarget target

bufferSubData :: (Storable a) => Target -> Int -> Natural -> [a] -> IO ()
bufferSubData target offset bytes values =
    withArray values $ glBufferSubData target' (fromIntegral offset) (fromIntegral bytes)
  where
    target' = fromBufferTarget target

fromBufferTarget :: Target -> GLenum
fromBufferTarget target = case target of
  ArrayBuffer -> gl_ARRAY_BUFFER
  IndexBuffer -> gl_ELEMENT_ARRAY_BUFFER
