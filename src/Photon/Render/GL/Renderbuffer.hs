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

module Photon.Render.GL.Renderbuffer where

import Control.Applicative
import Foreign.Concurrent
import Foreign.Marshal ( malloc, free )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Photon.Render.GL.GLObject
import Photon.Render.GL.Texture ( InternalFormat, fromInternalFormat )

newtype Renderbuffer = Renderbuffer { unRenderbuffer :: GLObject } deriving (Eq,Show)

genRenderbuffer :: IO Renderbuffer
genRenderbuffer = do
  p <- malloc
  glGenRenderbuffers 1 p
  Renderbuffer . GLObject <$> newForeignPtr p (glDeleteRenderbuffers 1 p >> free p)

bindRenderbuffer :: Renderbuffer -> IO ()
bindRenderbuffer (Renderbuffer rb) = withGLObject rb (glBindRenderbuffer gl_RENDERBUFFER)

unbindRenderbuffer :: IO ()
unbindRenderbuffer = glBindRenderbuffer gl_RENDERBUFFER 0

renderbufferStorage :: InternalFormat -> Natural -> Natural -> IO ()
renderbufferStorage ift w h = glRenderbufferStorage gl_RENDERBUFFER ift' (fromIntegral w) (fromIntegral h)
  where
    ift' = fromInternalFormat ift
