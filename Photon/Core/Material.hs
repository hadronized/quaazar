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
  , matColor
  , matShininess
  , matAlbedo
  ) where

import Control.Applicative
import Control.Lens ( makeLenses )
import Data.Aeson
import Photon.Core.Color ( Color )

-- |Material.
data Material = Material {
    -- |Color of the material.
    _matColor     :: Color
    -- |Shininess of the material. That property directly affects the
    -- specular aspect of the material. The greater it is, the intense
    -- the specular effect is.
  , _matShininess :: Float
    -- |Albedo of the material. That property controls the amount of
    -- reflected light the surface emits.
  , _matAlbedo    :: Float
  } deriving (Eq,Show)

instance FromJSON Material where
  parseJSON = withObject "material" $ \o ->
    Material <$> o .: "color" <*> o .: "shininess" <*> o .: "albedo"

makeLenses ''Material
