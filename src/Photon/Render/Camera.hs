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
import Linear ( (!*!), M44, V3 )
import Photon.Core.Entity ( Entity, entityPosition )
import Photon.Core.Projection ( Projection, projectionMatrix )
import Photon.Render.GL.Entity ( cameraTransform )
import Photon.Render.GL.Shader ( Uniform, (@=) )

data GPUCamera = GPUCamera {
    runCamera :: Uniform (M44 Float) -- ^ projection * view
              -> Uniform (V3 Float) -- ^ eye
              -> IO ()
  , cameraProjection :: M44 Float
  }

gpuCamera :: (Monad m) => Projection -> Entity -> m GPUCamera
gpuCamera proj ent = return (GPUCamera sendCamera proj')
  where
    sendCamera projViewU eyeU = do
      projViewU @= proj' !*! cameraTransform ent
      eyeU @= ent^.entityPosition
    proj' = projectionMatrix proj
