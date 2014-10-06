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

module Photon.Core.Material where

import Control.Lens ( makeLenses )
import Data.Aeson
import Photon.Core.Color ( Color )

-- |Material.
data Material = Material {
    _matColor :: Color
  } deriving (Eq,Show)

instance FromJSON Material where
  parseJSON = withObject "material" $ \o -> fmap Material (o .: "color")

makeLenses ''Material
