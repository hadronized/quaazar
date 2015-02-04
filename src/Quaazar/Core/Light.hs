{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Quaazar.Core.Light (
    -- * Light
    Light(..)
  ) where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Quaazar.Core.Color ( Color )
import Quaazar.Core.Loader ( Load(..) )

data Light
  = Omni
      Color -- ^ Light color
      Float -- ^ Light power
      Float -- ^ Light radius
      Bool  -- ^ Does the light cast shadow?
    deriving (Eq,Show)

instance FromJSON Light where
  parseJSON = withObject "light" $ \o -> do
      t :: String <- o .: "type"
      withType t o
    where
      withType t o
        | t == "omni" =
          Omni
            <$> o .: "color"
            <*> o .: "power"
            <*> o .: "radius"
            <*> o .:? "cast_shadows" .!= False
        | otherwise = fail "unknown light type"

makeLenses ''Light

instance Load Light where
  loadRoot = const "lights"
  loadExt = const "ylig"
