{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Quaazar.Animation.Key (
    -- * Key
    Key(..)
    -- * Re-exported
  , module X
  ) where

import Data.Aeson
import Linear
import Quaazar.Animation.Interpolation as X

data Key t a = Key {
    -- |
    keyIndex :: t
    -- |
  , keyInterpolation :: Interpolation
    -- |
  , keyValue :: a
    -- |
  , keyHandles :: Maybe (V2 a,V2 a)
  }

instance (FromJSON t,FromJSON a,FromJSON (V2 a)) => FromJSON (Key t a) where
  parseJSON = withObject "key" $ \o -> do
    index <- o .: "index"
    interpolation <- o .: "interpolation"
    value <- o .: "value"
    leftHandle <- o .: "left"
    rightHandle <- o .: "right"
    pure $ Key index interpolation value $ (,) <$> leftHandle <*> rightHandle
