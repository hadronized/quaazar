-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Timepoints are points in time as any other points in specific dimensions
-- (space, uvtex and so on). You can then get the difference between two
-- time points (you might be used to calling that “elapsed time”).

-- The whole thing can be used as clocking stuff.
----------------------------------------------------------------------------

module Photon.Utils.TimePoint (
    -- * Time point
    TimePoint
  , timePoint
  ) where

import Data.Time.Clock ( DiffTime, utctDayTime, getCurrentTime )

-- |Time point. This is a very important type used to represent absolute time.
-- Elapsed time between two time points simply gives you how many seconds were
-- stood between those points.
type TimePoint = DiffTime

-- |Get a time point.
timePoint :: IO TimePoint
timePoint = fmap utctDayTime getCurrentTime
