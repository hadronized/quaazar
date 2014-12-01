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
    -- * Reaction
  , LightSpawned(..)
  , LightLost(..)
  , LightEffect(..)
  , changeColor
  , changePower
  , changeRadius
  , changeCastShadows
  , withLight
  ) where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Photon.Core.Color ( Color )
import Photon.Core.Effect

data LightType
  = Omni
    deriving (Eq,Show)

instance FromJSON LightType where
  parseJSON = withText "light type" parseType
    where
      parseType t
        | t == "omni" = return Omni
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
  parseJSON = withObject "light" $ \o ->
    Light
      <$> o .: "type"
      <*> o .: "color"
      <*> o .: "power"
      <*> o .: "radius"
      <*> o .:? "cast_shadows" .!= False

makeLenses ''Light

data LightSpawned = LightSpawned (Managed Light) deriving (Eq,Show)

data LightLost = LightLost (Managed Light) deriving (Eq,Show)

data LightEffect
  = ColorChanged (Managed Light) Color
  | PowerChanged (Managed Light) Float
  | RadiusChanged (Managed Light) Float
  | CastShadowsChanged (Managed Light) Bool
  | UseLight (Managed Light)
  | UnuseLight (Managed Light)
    deriving (Eq,Show)

instance EffectfulManage Light LightSpawned LightLost where
  spawned = LightSpawned
  lost = LightLost

changeColor :: (Effect LightEffect m)
            => Managed Light
            -> (Color -> Color)
            -> m (Managed Light)
changeColor l f = do
    react (ColorChanged l newColor)
    return (l & managed . ligColor .~ newColor)
  where newColor = f (l^.managed.ligColor)

changePower :: (Effect LightEffect m)
            => Managed Light
            -> (Float -> Float)
            -> m (Managed Light)
changePower l f = do
    react (PowerChanged l newPower)
    return (l & managed . ligPower .~ newPower)
  where newPower = f (l^.managed.ligPower)

changeRadius :: (Effect LightEffect m)
            => Managed Light
            -> (Float -> Float)
            -> m (Managed Light)
changeRadius l f = do
    react (RadiusChanged l newRadius)
    return (l & managed . ligRadius .~ newRadius)
  where newRadius = f (l^.managed.ligRadius)

changeCastShadows :: (Effect LightEffect m)
                  => Managed Light
                  -> (Bool -> Bool)
                  -> m (Managed Light)
changeCastShadows l f = do
    react (CastShadowsChanged l newCastShadows)
    return (l & managed . ligCastShadows .~ newCastShadows)
  where newCastShadows = f (l^.managed.ligCastShadows)

withLight :: (Effect LightEffect m) => Managed Light -> m a -> m a
withLight lig a = react (UseLight lig) *> a <* react (UnuseLight lig)
