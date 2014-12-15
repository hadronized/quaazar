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

module Photon.Render.Camera where

import Control.Lens
import Linear.Matrix ( M44 )
import Linear.V3 ( V3 )
import Photon.Core.Entity ( Entity, entityPosition )
import Photon.Core.Projection ( Projection, projectionMatrix )
import Photon.Render.GL.Entity ( cameraTransform )
import Photon.Render.GL.Shader ( Uniform, (@=) )

data GPUCamera = GPUCamera {
    runCamera :: Uniform (M44 Float)
              -> Uniform (M44 Float)
              -> Uniform (V3 Float)
              -> Uniform (V3 Float)
              -> IO ()
  }

gpuCamera :: (Monad m) => Projection -> Entity -> m GPUCamera
gpuCamera proj ent = return . GPUCamera $ \proju viewu eyeu _ -> do
  proju @= projectionMatrix proj
  viewu @= cameraTransform ent
  eyeu @= ent^.entityPosition
  -- TODO: forward
