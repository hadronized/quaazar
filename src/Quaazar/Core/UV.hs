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

module Quaazar.Core.UV (
    -- * UV
    UV(..)
  , uv
  ) where

import Data.Aeson
import Data.Aeson.Types ( typeMismatch )
import Linear ( V2(..) )

newtype UV = UV { unUV :: V2 Float } deriving (Eq,Ord,Show)

instance FromJSON UV where
  parseJSON v' = do
    a <- parseJSON v'
    case a of
      [u,v] -> return (uv u v)
      _     -> typeMismatch "uv" v'

uv :: Float -> Float -> UV
uv u v = UV (V2 u v)
