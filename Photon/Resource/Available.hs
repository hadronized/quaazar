-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- When resources are loaded, they’re exposed to the user through a type
-- called 'Available', which is used to lookup resources from.
----------------------------------------------------------------------------

module Photon.Resource.Available where

import Control.Lens
import Data.Map as M ( Map, mapMaybe )
import Photon.Core.Light ( Light )
import Photon.Core.Mesh ( Mesh )
import Photon.Core.Model ( Model )

-- |Expose available resources. See 'Resource' for further details about
-- how to get resources from 'Available'.
data Available = Available {
    -- |Available meshes.
    _meshes           :: Map String Mesh
    -- |Available models.
  , _models           :: Map String Model
    -- |Available lights.
  , _lights           :: Map String Light
  }

makeLenses ''Available
