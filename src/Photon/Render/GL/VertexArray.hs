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

module Photon.Render.GL.VertexArray where

import Control.Applicative
import Data.Word ( Word8 )
import Foreign.Concurrent
import Foreign.Marshal ( malloc, free )
import Foreign.Marshal.Array ( advancePtr )
import Foreign.Marshal.Utils ( fromBool )
import Foreign.Ptr ( Ptr, nullPtr )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Photon.Render.GL.GLObject

newtype VertexArray = VertexArray { unVertexArray :: GLObject } deriving (Eq,Show)

data AttribType
  = Ints
  | Words
  | Floats
    deriving (Eq,Show)

genVertexArray :: IO VertexArray
genVertexArray = do
  p <- malloc
  glGenVertexArrays 1 p
  VertexArray . GLObject <$> newForeignPtr p (glDeleteVertexArrays 1 p >> free p)

genAttributelessVertexArray :: IO VertexArray
genAttributelessVertexArray = do
  va <- genVertexArray
  bindVertexArray va
  unbindVertexArray
  return va
  
bindVertexArray :: VertexArray -> IO ()
bindVertexArray (VertexArray va) = withGLObject va glBindVertexArray

unbindVertexArray :: IO ()
unbindVertexArray = glBindVertexArray 0

enableVertexAttrib :: Natural -> IO ()
enableVertexAttrib = glEnableVertexAttribArray . fromIntegral

vertexAttribPointer :: Natural -> Natural -> AttribType -> Bool -> Int -> IO ()
vertexAttribPointer attrib size atype normalized offset =
    glVertexAttribPointer (fromIntegral attrib) (fromIntegral size) atype' (fromBool normalized) 0 ptr
  where
    atype' = fromAttribType atype
    ptr    = nullPtr `advancePtr` offset :: Ptr Word8

fromAttribType :: AttribType -> GLenum
fromAttribType attrib = case attrib of
  Ints   -> gl_INT
  Words  -> gl_UNSIGNED_INT
  Floats -> gl_FLOAT
