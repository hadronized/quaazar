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

module Quaazar.Render.Frame where

import Control.Lens
import Control.Monad.Trans ( MonadIO(..) )
import Control.Monad.Error.Class ( MonadError )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Render.GL.Framebuffer (Target(..), bindFramebuffer )
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Texture ( Filter(..), Format(..), InternalFormat(..)
                                 , bindTextureAt )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

data GPUFrame = GPUFrame {
    useFrame :: IO ()
  , asTexture :: GPUTexture
  }

getScreenFrame :: (Monad m) => m GPUFrame
getScreenFrame =
  return $ GPUFrame (glBindFramebuffer gl_FRAMEBUFFER 0) (const $ return ())

gpuFrame :: (MonadScoped IO m, MonadIO m,MonadError Log m)
         => Natural
         -> Natural
         -> m GPUFrame
gpuFrame w h = do
    off <- genOffscreen w h Nearest RGB32F RGB
    return $ GPUFrame (bindFramebuffer (off^.offscreenFB) ReadWrite)
      (GPUTexture . bindTextureAt $ off^.offscreenRender)
