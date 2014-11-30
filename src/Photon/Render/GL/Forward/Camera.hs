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

module Photon.Render.OpenGL.Forward.Camera where

import Linear.Matrix ( M44 )
import Photon.Core.Projection ( Projection, projectionMatrix )

newtype FCamera = FCamera (M44 Float) deriving (Eq,Show)

forwardCamera :: Projection -> FCamera
forwardCamera = FCamera . projectionMatrix
