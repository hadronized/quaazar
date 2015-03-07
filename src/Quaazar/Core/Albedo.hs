{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Quaazar.Core.Albedo (
    -- * Albedo
    Albedo(..)
  , albedo
  ) where

import Data.Aeson
import Data.Aeson.Types ( typeMismatch )
import Linear.V3

newtype Albedo = Albedo { unAlbedo :: V3 Float } deriving (Eq,Ord,Show)

instance FromJSON Albedo where
  parseJSON v = do
    x <- parseJSON v
    case x of
      [r,g,b] -> return (albedo r g b)
      _       -> typeMismatch "albedo" v

albedo :: Float -> Float -> Float -> Albedo
albedo r g b = Albedo (V3 r g b)
