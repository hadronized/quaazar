-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Normal in space is a 3-float vector.
----------------------------------------------------------------------------

module Photon.Core.Normal (
    -- * Normal 
    Normal(..)
  , nor
  ) where

import Data.Aeson
import Linear ( V3(..) )

newtype Normal = Normal { unNormal :: V3 Float } deriving (Eq,Ord,Show)

instance FromJSON Normal where
  parseJSON v = do
    [x,y,z] <- parseJSON v
    return $ nor x y z

nor :: Float -> Float -> Float -> Normal
nor x y z = Normal (V3 x y z)
