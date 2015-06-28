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

module Quaazar.Animation.Interpolation (
    -- * Interpolation mode
    Interpolation(..)
  ) where

import Data.Aeson

data Interpolation
  = Constant
  | Linear
  | CubicHermite
  | Bezier
    deriving (Eq,Show)

instance FromJSON Interpolation where
  parseJSON = withText "interpolation mode" parse
    where
      parse t
        | t == "CONSTANT" = pure Constant
        | t == "LINEAR" = pure Linear
        | t == "CUBIC" = pure CubicHermite
        | t == "BEZIER" = pure Bezier
        | otherwise = fail "unknown interpolation mode"
