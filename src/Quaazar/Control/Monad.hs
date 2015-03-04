-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- This module enhances a little bit "Control.Monad" by adding a few useful
-- functions.
----------------------------------------------------------------------------

module Quaazar.Control.Monad (
    -- * Filters
    partitionM
  ) where

-- |Monadic version of 'filter'.
partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM p l = sel l ([],[])
  where
    sel [] a = return a
    sel (x:xs) (ts,fs) = do
        r <- p x
        sel xs $ if r then (x:ts,fs) else (ts,x:fs)
