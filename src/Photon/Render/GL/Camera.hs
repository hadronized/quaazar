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

module Photon.Render.GL.Camera where

import Linear.Matrix ( M44 )
import Photon.Core.Projection ( Projection, projectionMatrix )

gpuProjection :: Projection -> M44 Float
gpuProjection = projectionMatrix
