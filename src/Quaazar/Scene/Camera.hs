-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Quaazar.Scene.Camera (
    -- * Camera type
    Camera
    -- * Re-exported modules
  , module Quaazar.Render.Projection
  , module Quaazar.Scene.Hierarchy
  ) where

import Quaazar.Render.Projection
import Quaazar.Scene.Hierarchy

type Camera = Instance Projection
