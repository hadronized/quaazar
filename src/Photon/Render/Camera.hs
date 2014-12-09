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

module Photon.Render.Camera (
    -- *
  ) where

import Photon.Core.Entity ( Entity )
import Photon.Core.Projection ( Projection )
import Photon.Render.GL.Entity ( cameraTransform )
import Photon.Render.Semantics ( cameraProjectionSem, cameraViewSem )
import Photon.Render.Shader

data GPUCamera = GPUCamera { runGPUCamera :: GPUProgram -> IO () }

gpuCamera :: (Monad m) => Projection -> Entity -> m GPUCamera
gpuCamera proj ent = return . GPUCamera $ \program -> do
  let sem = programSemantic program
  sem cameraProjectionSem @?= projectionMatrix proj
  sem cameraViewSem @?= cameraTransform ent
