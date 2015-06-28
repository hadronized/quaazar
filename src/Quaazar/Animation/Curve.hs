-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Quaazar.Animation.Curve (
    -- * Curve
    Curve
    -- * Re-exported
  , module X
  ) where

import Quaazar.Animation.Key as X

newtype Curve t a = Curve { unCurve :: [Key t a] }
