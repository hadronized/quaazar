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
  , Resource(..)
  ) where

import Control.Lens ( makeLenses )
import Data.Map as M ( Map )
import Photon.Core.Light ( Light )
import Photon.Core.Mesh ( Mesh )
import Photon.Core.Model ( Model )
import Photon.Core.Vertex ( VertexFormat )

-- |Expose available resources. See 'Resource' for further details about
-- how to get resources from 'Available'.
data Available n = Available {
    -- |Available vertex formats.
    _vertexFormats :: Map n VertexFormat
    -- |Available meshes.
  , _meshes        :: Map n Mesh
    -- |Available models.
  , _models        :: Map n Model
    -- |Available lights.
  , _lights        :: Map n Light
  } deriving (Eq,Show)

makeLenses ''Available

-- |This typeclass provides 'resource', a function used with 'Available'
-- to extract resources.
--
-- Once you got an 'Available' value, you can use the 'Maybe' *monad*
-- to build your scene, since 'resource' outputs in 'Maybe', and you
-- don’t want to go on if a resource is missing. If you do, you can
-- use some kind of 'MonadError' monad.
class Resource a where
  resource :: Available n -> n > Maybe a
