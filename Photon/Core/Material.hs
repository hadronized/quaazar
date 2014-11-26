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
  , MaterialSpawned(..)
  , MaterialLost(..)
  , MaterialEffect(..)
  , changeDiffuse
  , changeSpecular
  , changeShininess
    -- * Albedo
  , Albedo(..)
  , albedo
  ) where

import Control.Applicative
import Control.Lens ( makeLenses )
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

data MaterialSpawned = MaterialSpawned (Managed Material) deriving (Eq,Show)

data MaterialLost = MaterialLost (Managed Material) deriving (Eq,Show)

data MaterialEffect
  = DiffuseChanged (Managed Material) Albedo
  | SpecularChanged (Managed Material) Albedo
  | ShininessChanged (Managed Material) Float
    deriving (Eq,Show)

instance EffectfulManage Material MaterialSpawned MaterialLost where
  spawned = MaterialSpawned
  lost = MaterialLost

changeDiffuse :: (Effect MaterialEffect m)
              => Managed Material
              -> (Albedo -> Albedo)
              -> m (Managed Material)
changeDiffuse m f = do
    react (DiffuseChanged m newDiffuse)
    return (m . matDiffuseAlbedo .~ newDiffuse)
  where newDiffuse = f (m^.matDiffuseAlbedo)

changeSpecular :: (Effect MaterialEffect m)
               => Managed Material
               -> (Albedo -> Albedo)
               -> m (Managed Material)
changeSpecular m f = do
    react (SpecularChanged m newDiffuse)
    return (m . matSpecularAlbedo .~ newSpecular)
  where newSpecular = f (m^.matSpecularAlbedo)

changeShininess :: (Effect MaterialEffect m)
                => Managed Material
                -> (Albedo -> Albedo)
                -> m (Managed Material)
changeShininess m f = do
    react (ShininessChanged m newShn)
    return (m . matShininess .~ newShn)
  where newShn = f (m^.matShininess)
