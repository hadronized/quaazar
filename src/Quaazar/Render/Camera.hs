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

module Quaazar.Render.Camera where

import Control.Lens
import Data.Maybe ( fromJust )
import Linear ( M44, V3, (!*!), inv44 )
import Quaazar.Core.Entity ( Entity, entityPosition )
import Quaazar.Core.Projection ( Projection, projectionMatrix )
import Quaazar.Render.GL.Entity ( cameraTransform )
import Quaazar.Render.GL.Shader ( Uniform, (@=) )

data GPUCamera = GPUCamera {
    runCamera :: Uniform (M44 Float) -- ^ projection * view
              -> Uniform (M44 Float) -- ^ (projection * view)-1
              -> Uniform (V3 Float) -- ^ eye
              -> IO ()
  , cameraProjection :: M44 Float
  }

gpuCamera :: (Monad m) => Projection -> Entity -> m GPUCamera
gpuCamera proj ent = return (GPUCamera sendCamera proj')
  where
    sendCamera projViewU iProjViewU eyeU = do
        projViewU @= projView
        iProjViewU @= fromJust (inv44 projView)
        eyeU @= ent^.entityPosition
    projView = proj' !*! cameraTransform ent
    proj' = projectionMatrix proj
