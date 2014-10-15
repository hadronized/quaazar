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
    Light(..)
    -- * Light properties
  , LightProperties(LightProperties)
  , ligColor
  , ligPower
  , ligRadius
  , ligCastShadows
  ) where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Photon.Core.Color ( Color )

-- |Light. Extra information (cuttoff angle for instance) can be added
-- regarding the type of the light.
data Light
  = Omni LightProperties -- ^ Omni light
    deriving (Eq,Show)

instance FromJSON Light where
  parseJSON = withObject "light" $ \o -> do
      t  <- o .: "type"
      lp <- o .: "properties"
      withText "light type" (parseType lp) t
    where
      parseType lp t
        | t == "omni" = return (Omni lp)
        | otherwise   = fail "unknown light type"

-- |Lighting properties. This type is shared by lights.
data LightProperties = LightProperties {
    -- |Color of the light.
    _ligColor       :: Color
    -- |Power of the light – a.k.a. intensity.
  , _ligPower       :: Float
    -- |Radius of the light.
  , _ligRadius      :: Float
    -- |Does the light cast shadows?
  , _ligCastShadows :: Bool
  } deriving (Eq,Show)

instance FromJSON LightProperties where
  parseJSON = withObject "light properties" $ \o ->
    LightProperties <$> o .: "color" <*> o .: "power" <*> o .: "radius" <*> o .:? "cast_shadows" .!= False

makeLenses ''LightProperties
