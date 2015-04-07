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
    -- * Shadows
  , ShadowLOD(..)
    -- * Resources
  , AmbientManager
  , OmniManager
  ) where

import Control.Applicative
import Data.Aeson
import Quaazar.Core.Color ( Color )
import Quaazar.Core.Loader ( Load(..) )
import Quaazar.Core.Resource ( Manager, Resource )

-- |'Ambient col pow'.
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
data Omni = Omni Color Float Float (Maybe ShadowLOD) deriving (Eq,Show)

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
            <*> o .:? "shadow_lod" .!= Nothing
        | otherwise = fail "not an omni light"

instance Load Omni where
  loadRoot = const "lights"
  loadExt = const "qlig"

instance Resource () Omni

type OmniManager = Manager () Omni

-- |'ShadowLOD' represents the level of detail of a shadow. There’s currently
-- three levels of detail:
--
--   - 'LowShadow' is a /low-detail/ shadow. You should use that LOD when you
--     don’t need details (distant shadows or very big shadows);
--   - 'MediumShadow' is a /medium-detail/ shadow. Used for common shadowing;
--   - 'HighShadow' is a /high-detail/ shadow. You can use that LOD for close
--     and/or small shadows the user might see pretty well.
data ShadowLOD
  = LowShadow
  | MediumShadow
  | HighShadow
    deriving (Eq,Ord,Read,Show)

instance FromJSON ShadowLOD where
  parseJSON = withText "shadow level of detail" parseText
    where
      parseText t
        | t == "low"    = return LowShadow
        | t == "medium" = return MediumShadow
        | t == "high"   = return HighShadow
        | otherwise     = fail "unknown shadow level of detail"
