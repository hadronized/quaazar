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

import Control.Monad.Trans ( MonadIO(..) )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Render.GL.GLObject
import Quaazar.Render.GL.Texture ( InternalFormat, fromInternalFormat )

newtype Renderbuffer = Renderbuffer { unRenderbuffer :: GLuint } deriving (Eq,Show)

instance GLObject Renderbuffer where
  genObjects n = genericGenObjects n glGenRenderbuffers glDeleteRenderbuffers Renderbuffer

bindRenderbuffer :: (MonadIO m) => Renderbuffer -> m ()
bindRenderbuffer (Renderbuffer rb) = liftIO $ glBindRenderbuffer gl_RENDERBUFFER rb

unbindRenderbuffer :: (MonadIO m) => m ()
unbindRenderbuffer = liftIO $ glBindRenderbuffer gl_RENDERBUFFER 0

renderbufferStorage :: (MonadIO m) => InternalFormat -> Natural -> Natural -> m ()
renderbufferStorage ift w h = liftIO $ glRenderbufferStorage gl_RENDERBUFFER ift' (fromIntegral w) (fromIntegral h)
  where
    ift' = fromInternalFormat ift
