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

module Quaazar.Render.GL.Renderbuffer where

import Foreign.Marshal ( alloca )
import Foreign.Marshal.Array ( peekArray, withArrayLen )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Render.GL.GLObject
import Quaazar.Render.GL.Texture ( InternalFormat, fromInternalFormat )

newtype Renderbuffer = Renderbuffer { unRenderbuffer :: GLuint } deriving (Eq,Show)

instance GLObject Renderbuffer where
  genObjects n = alloca $ \p -> do
    glGenRenderbuffers (fromIntegral n) p
    fmap (map Renderbuffer) $ peekArray n p
  deleteObjects a = withArrayLen (map unRenderbuffer a) $ \s p ->
    glDeleteRenderbuffers (fromIntegral s) p

bindRenderbuffer :: Renderbuffer -> IO ()
bindRenderbuffer (Renderbuffer rb) = glBindRenderbuffer gl_RENDERBUFFER rb

unbindRenderbuffer :: IO ()
unbindRenderbuffer = glBindRenderbuffer gl_RENDERBUFFER 0

renderbufferStorage :: InternalFormat -> Natural -> Natural -> IO ()
renderbufferStorage ift w h = glRenderbufferStorage gl_RENDERBUFFER ift' (fromIntegral w) (fromIntegral h)
  where
    ift' = fromInternalFormat ift
