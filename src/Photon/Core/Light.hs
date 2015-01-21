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
-- Lighting is required in order to see non-emissive objects.
--
-- 'Light' exposes the light type. You can find these types of light:
--
--   - 'Omni': omni lights – a.k.a. point lights – are lights that emit in
--     all directions
--
-- Whatever the type of a light, it holds lighting information via a value
-- of type 'LightProperties'.
----------------------------------------------------------------------------

module Photon.Core.Light (
    -- * Light
    Light(Light)
  , LightType(..)
  , ligType
  , ligColor
  , ligPower
  , ligRadius
  , ligCastShadows
  ) where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Photon.Core.Color ( Color )
import Photon.Core.Loader ( Load(..) )

data LightType
  = Omni
  | Ambient
    deriving (Eq,Show)

instance FromJSON LightType where
  parseJSON = withText "light type" parseType
    where
      parseType t
        | t == "omni" = return Omni
        | t == "ambient" = return Ambient
        | otherwise = fail "unknown light type"

data Light = Light {
    -- |Type of the light.
    ligType         :: LightType
    -- |Color of the light.
  , _ligColor       :: Color
    -- |Power of the light – a.k.a. intensity.
  , _ligPower       :: Float
    -- |Radius of the light.
  , _ligRadius      :: Float
    -- |Does the light cast shadows?
  , _ligCastShadows :: Bool
  } deriving (Eq,Show)

instance FromJSON Light where
  parseJSON = withObject "light" $ \o -> do
    t <- o .: "type"
    c <- o .: "color"
    case t of
      Omni -> do
        Light
          <$> pure t
          <*> pure c
          <*> o .: "power"
          <*> o .: "radius"
          <*> o .:? "cast_shadows" .!= False
      Ambient -> do
        Light
          <$> pure t
          <*> pure c
          <*> pure 0
          <*> pure 0
          <*> pure False
    Light
      <$> o .: "type"
      <*> o .: "color"
      <*> o .: "power"
      <*> o .: "radius"
      <*> o .:? "cast_shadows" .!= False

makeLenses ''Light

instance Load Light where
  loadRoot = const "lights"
  loadExt = const "ylig"
