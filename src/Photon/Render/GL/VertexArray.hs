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

import Data.Word ( Word8 )
import Foreign.Marshal ( alloca )
import Foreign.Marshal.Array ( advancePtr, peekArray, withArrayLen )
import Foreign.Marshal.Utils ( fromBool )
import Foreign.Ptr ( Ptr, nullPtr )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Photon.Render.GL.GLObject

newtype VertexArray = VertexArray { unVertexArray :: GLuint } deriving (Eq,Ord,Show)

instance GLObject VertexArray where
  genObjects n = alloca $ \p -> do
    glGenVertexArrays (fromIntegral n) p
    fmap (map VertexArray) $ peekArray n p
  deleteObjects a = withArrayLen (map unVertexArray a) $ \s p ->
    glDeleteVertexArrays (fromIntegral s) p

data AttribType
  = Ints
  | Words
  | Floats
    deriving (Eq,Show)

genAttributelessVertexArray :: IO VertexArray
genAttributelessVertexArray = do
  va <- genObject
  bindVertexArray va
  unbindVertexArray
  return va

bindVertexArray :: VertexArray -> IO ()
bindVertexArray (VertexArray va) = glBindVertexArray va

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
