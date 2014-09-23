-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Position in space is a 3-float vector.
----------------------------------------------------------------------------

module Photon.Core.Position (
    -- * Position
    Position(..)
  ) where

import Data.Aeson
import Linear ( V3(..) )

newtype Position = Position { unPosition :: V3 Float } deriving (Eq,Ord,Show)

instance FromJSON Position where
  parseJSON v = do
    [x,y,z] <- parseJSON v
    return . Position $ V3 x y z
