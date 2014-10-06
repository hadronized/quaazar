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

module Photon.Resource.Available (
    -- * Available resources
    Available
  , empty
  , meshes
  , materials
  , lights
  ) where

import Control.Lens
import Data.Map ( Map )
import Photon.Core.Light ( Light )
import Photon.Core.Material ( Material )
import Photon.Core.Mesh ( Mesh )
import qualified Data.Map as M ( empty )

-- |Expose available resources. See 'Resource' for further details about
-- how to get resources from 'Available'.
data Available = Available {
    -- |Available meshes.
    _meshes    :: Map String Mesh
    -- |Available materials.
  , _materials :: Map String Material
    -- |Available lights.
  , _lights    :: Map String Light
  }

empty :: Available
empty = Available M.empty M.empty M.empty

makeLenses ''Available
