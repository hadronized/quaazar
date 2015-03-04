-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
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

module Quaazar.Utils.TimePoint (
    -- * Time point
    timePoint
  ) where

import System.Clock ( Clock(Monotonic), TimeSpec(..), getTime )

-- |Get a time point.
timePoint :: IO Double
timePoint = do
  TimeSpec secs nsecs <- getTime Monotonic
  return $ fromIntegral secs + fromIntegral nsecs / (1000000000 :: Double)
