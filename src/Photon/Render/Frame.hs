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

module Photon.Render.Frame where

import Control.Lens
import Control.Monad.Trans ( MonadIO(..) )
import Control.Monad.Error.Class ( MonadError )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Photon.Render.GL.Framebuffer ( AttachmentPoint(..), Target(..)
                                    , bindFramebuffer )
import Photon.Render.GL.Offscreen
import Photon.Render.GL.Texture ( Format(..), InternalFormat(..)
                                , bindTextureAt )
import Photon.Utils.Either ( generalizeEither )
import Photon.Utils.Log

data GPUFrame = GPUFrame {
    useFrame :: IO ()
  , bindFrameAt :: Natural -> IO ()
  }

getScreenFrame :: (Monad m) => m GPUFrame
getScreenFrame =
  return $ GPUFrame (glBindFramebuffer gl_FRAMEBUFFER 0) undefined

gpuFrame :: (MonadIO m,MonadError Log m) => Natural -> Natural -> m GPUFrame
gpuFrame w h = do
    off <- genOffscreen w h RGB32F RGB
    return $ GPUFrame (bindFramebuffer (off^.offscreenFB) ReadWrite)
      (bindTextureAt $ off^.offscreenRender)
