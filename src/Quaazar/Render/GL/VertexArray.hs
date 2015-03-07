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

module Quaazar.Render.GL.VertexArray where

import Control.Monad.Trans ( MonadIO(..) )
import Data.Word ( Word8 )
import Foreign.Marshal.Array ( advancePtr )
import Foreign.Marshal.Utils ( fromBool )
import Foreign.Ptr ( Ptr, nullPtr )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Render.GL.GLObject

newtype VertexArray = VertexArray { unVertexArray :: GLuint } deriving (Eq,Ord,Show)

instance GLObject VertexArray where
  genObjects n =
    genericGenObjects n glGenVertexArrays glDeleteVertexArrays VertexArray

data AttribType
  = Ints
  | Words
  | Floats
    deriving (Eq,Show)

genAttributelessVertexArray :: (MonadScoped IO m) => m VertexArray
genAttributelessVertexArray = do
  va <- genObject
  liftBase $ do
    bindVertexArray va
    unbindVertexArray
    return va

bindVertexArray :: (MonadIO m) => VertexArray -> m ()
bindVertexArray (VertexArray va) = liftIO $ glBindVertexArray va

unbindVertexArray :: (MonadIO m) => m ()
unbindVertexArray = liftIO $ glBindVertexArray 0

enableVertexAttrib :: (MonadIO m) => Natural -> m ()
enableVertexAttrib = liftIO . glEnableVertexAttribArray . fromIntegral

vertexAttribPointer :: (MonadIO m) => Natural -> Natural -> AttribType -> Bool -> Int -> m ()
vertexAttribPointer attrib size atype normalized offset = liftIO $
    glVertexAttribPointer (fromIntegral attrib) (fromIntegral size) atype' (fromBool normalized) 0 ptr
  where
    atype' = fromAttribType atype
    ptr    = nullPtr `advancePtr` offset :: Ptr Word8

fromAttribType :: AttribType -> GLenum
fromAttribType attrib = case attrib of
  Ints   -> gl_INT
  Words  -> gl_UNSIGNED_INT
  Floats -> gl_FLOAT
