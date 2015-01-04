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
    -- * Material and layers
    Material(..)
  , material
  , MaterialLayer(MaterialLayer)
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
import Data.List.NonEmpty ( toList )
import Data.Semigroup ( Semigroup(..) )
import Linear.V3

newtype Albedo = Albedo { unAlbedo :: V3 Float } deriving (Eq,Ord,Show)

instance FromJSON Albedo where
  parseJSON v = do
    x <- parseJSON v
    case x of
      [r,g,b] -> return (albedo r g b)
      _       -> typeMismatch "albedo" v

-- |Material layer.
data MaterialLayer = MaterialLayer {
    -- |Diffuse albedo of the material layer.
    _matDiffuseAlbedo  :: Albedo
    -- |Specular albedo of the material layer.
  , _matSpecularAlbedo :: Albedo
    -- |Shininess of the material layer. That property directly affects the
    -- specular aspect of the material. The greater it is, the intense
    -- the specular effect is.
  , _matShininess      :: Float
  } deriving (Eq,Show)

instance FromJSON MaterialLayer where
  parseJSON = withObject "material layer" $ \o ->
    MaterialLayer <$> o .: "diffalb" <*> o .: "specalb" <*> o .: "shininess"

newtype Material = Material { materialLayers :: [MaterialLayer] }

instance FromJSON Material where
  parseJSON a = do
    layers <- parseJSON a
    case layers of
      [] -> fail "a material should have at least one layer"
      _ -> return (Material layers)

instance Semigroup Material where
  Material a <> Material b = Material (a ++ b)
  sconcat = Material . concatMap materialLayers . toList

material :: Albedo -> Albedo -> Float -> Material
material diff spec shn = Material [MaterialLayer diff spec shn]

albedo :: Float -> Float -> Float -> Albedo
albedo r g b = Albedo (V3 r g b)

makeLenses ''MaterialLayer
