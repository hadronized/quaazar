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
--   - 'Ambient` : basic flat ambient lighting – often used to adjust the
--     exposure of a scene
--   - 'Omni': omnidirectional lights – a.k.a. point lights – are lights that
--     emit in all directions
----------------------------------------------------------------------------

module Photon.Core.Light (
    -- * Light
    Light(..)
  ) where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Photon.Core.Color ( Color )
import Photon.Core.Loader ( Load(..) )

data Light
  = Ambient
      Color -- ^ Light color
      Float -- ^ Light power
  | Omni
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
        | t == "ambient" =
          Ambient
            <$> o .: "color"
            <*> o .: "power"
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
