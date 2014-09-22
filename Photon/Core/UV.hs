-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- UV coordinates are represented by a 2D-float vector.
----------------------------------------------------------------------------

module Photon.Core.UV (
    -- * UV
    UV
  ) where

import Linear ( V3 )

type UV = V2 Float
