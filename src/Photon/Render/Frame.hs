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
import Numeric.Natural ( Natural )
import Photon.Render.GL.Framebuffer ( AttachmentPoint(..), Target(..)
                                    , bindFramebuffer )
import Photon.Render.GL.Offscreen
import Photon.Render.GL.Texture ( Format(..), InternalFormat(..)
                                , bindTextureAt )
import Photon.Utils.Log

data GPUFrame = GPUFrame {
    useFrame :: IO ()
  , bindFrameAt :: Natural -> IO ()
  }

gpuFrame :: Natural -> Natural -> IO (Either Log GPUFrame)
gpuFrame w h = do
    off <- genOffscreen w h RGB32F RGB (ColorAttachment 0) Depth32F
             DepthAttachment
    return $ either Left gpuFrame_ off
  where
    gpuFrame_ off =
      Right $ GPUFrame (bindFramebuffer (off^.offscreenFB) ReadWrite)
        (bindTextureAt $ off^.offscreenTex)
