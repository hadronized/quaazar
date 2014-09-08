-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Models are a way to customize meshes. They add the concept of *material*
-- and are useful to customize the way a mesh is rendered.
----------------------------------------------------------------------------

module Photon.Core.Model where

import Control.Lens ( makeLenses )
import Photon.Core.Color ( Color )

-- |Model.
data Model = Model {
    _modelColor :: Color
  } deriving (Eq,Show)

makeLenses ''Model
