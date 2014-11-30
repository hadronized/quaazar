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

module Photon.Render.OpenGL.Forward.Material where

import Photon.Core.Material ( Material(..) )

newtype FMaterial = FMaterial { unFMaterial :: Material } deriving (Eq,Show)

forwardMaterial :: Material -> FMaterial
forwardMaterial = FMaterial
