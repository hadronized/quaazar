{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Lighting enables shading computation over surfaces with light sources.
--
-- 'Ambient' lighting is a very simple kind of lighting. It has no source.
-- It only has a 'Color' that defines the ambient spectrum all over the
-- scene, and a power used to modulate that color. That color is then
-- diffused to all objects, no matter what. Because of 'Ambient' not having
-- a source in space, no diffuse nor specular hilights are possible.
--
-- 'Omni' lights are the default lights. They’re omnidirectional lights
-- emitting photons in all directions from their sources. They’re defined
-- via a 'Color', a power, a radius and a shadow configuration.
----------------------------------------------------------------------------

module Quaazar.Lighting.Light (
    -- * Lights
    Ambient(..)
  , Omni(..)
  ) where

import Data.Aeson
import Data.Semigroup ( Semigroup(..) )
import Quaazar.Lighting.Shadow
import Quaazar.Scene.Color ( Color, color )
import Quaazar.System.Loader ( Load(..) )

-- |'Ambient col pow' is the ambient lighting of
-- scene.
--
-- If you want a perfect dark scene, you can use the
-- following ambient lights:
--
-- @
--   Ambient (color 0 0 0) whatever
--   Ambient whatever 0
--   Ambient (color 0 0 0) 0
-- @
--
-- In no circumstances you’d want to use @Ambient (color 1 1 1) 1@
-- as it would burn your whole scene!
data Ambient = Ambient Color Float deriving (Eq,Show)

instance FromJSON Ambient where
  parseJSON = withObject "ambient light" $ \o -> do
      t :: String <- o .: "type"
      withType t o
    where
      withType t o
        | t == "ambient" = Ambient <$> o .: "color" <*> o .: "power"
        | otherwise = fail "not an ambient light"

instance Load () Ambient where
  loadRoot = const "lights"
  loadExt = const "ylig"

instance Monoid Ambient where
  mempty = Ambient (color 0 0 0) 0
  mappend = (<>)

instance Semigroup Ambient where
  Ambient ca pa <> Ambient cb pb = Ambient (ca + cb) (pa + pb)
  
-- |'Omni col pow rad shadowLOD' is an omnidirectional light.
--
-- 'shadowLOD' is an object of type @Maybe ShadowLOD@. Feel free to read
-- the documentation of 'ShadowLOD' for further information about that type.
-- If you don’t want the light to cast shadows, set 'shadowLOD' to 'Nothing.
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

instance Load () Omni where
  loadRoot = const "lights"
  loadExt = const "qlig"
