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

module Photon.Render.Frame (
    -- *
  ) where

import Control.Lens
import Numeric.Natural ( Natural )
import Photon.Render.GL.Offscreen ( Offscreen, genOffscreen )

newtype GPUFrame = GPUFrame { 
    useFrame :: IO ()
  , bindFrameAt :: Natural -> IO ()
  } deriving (Eq)

gpuFrame :: (MonadIO m) => m GPUFrame
gpuFrame = do
  off <- genOffscreen
  return $ GPUFrame (bindFramebuffer $ off^.offscreenFB) (bindTextureAt $ off^.offscreenTex)
