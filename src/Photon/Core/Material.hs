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
-- Materials are a way to customize objects. They add the concept of *material*
-- and are useful to customize the way a mesh is rendered.
----------------------------------------------------------------------------

module Photon.Core.Material (
    -- * Material
    Material(Material)
  , matDiffuseAlbedo
  , matSpecularAlbedo
  , matShininess
    -- * Albedo
  , Albedo(..)
  , albedo
  ) where

import Control.Applicative
import Control.Lens
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
-- |Material.
data Material = Material {
    -- |Diffuse albedo of the material.
    _matDiffuseAlbedo  :: Albedo 
    -- |Specular albedo of the material.
  , _matSpecularAlbedo :: Albedo
    -- |Shininess of the material. That property directly affects the
    -- specular aspect of the material. The greater it is, the intense
    -- the specular effect is.
  , _matShininess      :: Float
  } deriving (Eq,Show)

instance FromJSON Material where
  parseJSON = withObject "material" $ \o ->
    Material <$> o .: "diffalb" <*> o .: "specalb" <*> o .: "shininess"

albedo :: Float -> Float -> Float -> Albedo
albedo r g b = Albedo (V3 r g b)

makeLenses ''Material
