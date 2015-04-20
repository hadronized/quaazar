-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Normal in space is a 3-float vector.
----------------------------------------------------------------------------

module Quaazar.Geometry.Normal (
    -- * Normal 
    Normal(..)
  , nor
  ) where

import Data.Aeson
import Data.Aeson.Types ( typeMismatch )
import Linear ( V3(..) )

-- |Normal. Used for lighting purposes mostly.
--
-- Notice: the 'nor' function is used to build a 'Normal', but don’t
-- get it wrong: the resulting 'Normal' **is not normalized**!
newtype Normal = Normal { unNormal :: V3 Float } deriving (Eq,Ord,Show)

instance FromJSON Normal where
  parseJSON v = do
    a <- parseJSON v
    case a of
      [x,y,z] -> return (nor x y z)
      _       -> typeMismatch "normal" v

-- |Build a 'Normal' from /x/, /y/ and /z/ components. Keep in mind that
-- the resulting 'Normal' is not normalized by 'nor': you have to handle
-- normalization.
nor :: Float -> Float -> Float -> Normal
nor x y z = Normal (V3 x y z)
