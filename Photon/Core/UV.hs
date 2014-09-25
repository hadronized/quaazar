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
    UV(..)
  , uv
  ) where

import Data.Aeson
import Linear ( V2(..) )

newtype UV = UV { unUV :: V2 Float } deriving (Eq,Ord,Show)

instance FromJSON UV where
  parseJSON v' = do
    [u,v] <- parseJSON v'
    return . UV $ V2 u v

uv :: Float -> Float -> UV
uv u v = UV (V2 u v)
