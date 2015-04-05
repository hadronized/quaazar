{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Quaazar.Core.Light (
    -- * Lights
    Ambient(..)
  , Omni(..)
    -- * Resources
  , AmbientManager
  , OmniManager
  ) where

import Control.Applicative
import Data.Aeson
import Quaazar.Core.Color ( Color )
import Quaazar.Core.Loader ( Load(..) )
import Quaazar.Core.Resource ( Manager, Resource )

data Ambient = Ambient Color Float deriving (Eq,Show)

instance FromJSON Ambient where
  parseJSON = withObject "ambient light" $ \o -> do
      t :: String <- o .: "type"
      withType t o
    where
      withType t o
        | t == "ambient" = Ambient <$> o .: "color" <*> o .: "power"
        | otherwise = fail "not an ambient light"

instance Load Ambient where
  loadRoot = const "lights"
  loadExt = const "ylig"

instance Resource () Ambient

type AmbientManager = Manager () Ambient

-- |'Omni col pow rad'.
data Omni = Omni Color Float Float Bool deriving (Eq,Show)

instance FromJSON Omni where
  parseJSON = withObject "omni light" $ \o -> do
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
        | otherwise = fail "not an omni light"

instance Load Omni where
  loadRoot = const "lights"
  loadExt = const "qlig"

instance Resource () Omni

type OmniManager = Manager () Omni
