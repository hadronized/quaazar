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
import Data.Maybe ( fromJust )
import Linear ( M44, V3, (!*!), inv44 )
import Photon.Core.Entity ( Entity, entityPosition )
import Photon.Core.Projection ( Projection, projectionMatrix )
import Photon.Render.GL.Entity ( cameraTransform )
import Photon.Render.GL.Shader ( Uniform, (@=) )
import Photon.Render.GPU

data GPUCamera = GPUCamera {
    runCamera :: Uniform (M44 Float) -- ^ projection * view
              -> Uniform (M44 Float) -- ^ (projection * view)-1
              -> Uniform (V3 Float) -- ^ eye
              -> IO ()
  , cameraProjection :: M44 Float
  }

instance GPU (Projection,Entity) GPUCamera where
  gpu = uncurry gpuCamera

gpuCamera :: (Monad m) => Projection -> Entity -> m GPUCamera
gpuCamera proj ent = return (GPUCamera sendCamera proj')
  where
    sendCamera projViewU iProjViewU eyeU = do
        projViewU @= projView
        iProjViewU @= fromJust (inv44 projView)
        eyeU @= ent^.entityPosition
    projView = proj' !*! cameraTransform ent
    proj' = projectionMatrix proj
