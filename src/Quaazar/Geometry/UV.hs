-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- UV coordinates are represented by a 2D-float vector.
----------------------------------------------------------------------------

module Quaazar.Geometry.UV (
    -- * UV
    UV(..)
  , uv
  ) where

import Data.Aeson
import Data.Aeson.Types ( typeMismatch )
import Linear ( V2(..) )

-- |UV texture coordinates. The system used for UV mapping is as following:
--
--   - /(0,0)/ is at upper-left
--   - /(1,0)/ is at upper-right
--   - /(0,1)/ is at lower-left
--   - /(1,1)/ is at lower-right
newtype UV = UV { unUV :: V2 Float } deriving (Eq,Ord,Show)

instance FromJSON UV where
  parseJSON v' = do
    a <- parseJSON v'
    case a of
      [[u,v]] -> return (uv u v)
      _     -> typeMismatch "uv" v'

-- |Build a 'UV' with /u/ and /v/ components.
uv :: Float -> Float -> UV
uv u v = UV (V2 u v)
