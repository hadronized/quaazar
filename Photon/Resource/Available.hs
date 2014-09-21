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
import Photon.Core.Vertex ( VertexFormat )
import Photon.Resource.Mesh ( UnresolvedMesh(..) )

-- |Expose available resources. See 'Resource' for further details about
-- how to get resources from 'Available'.
data Available n = Available {
    -- |Available vertex formats.
    _vertexFormats    :: Map n VertexFormat
    -- |Available meshes.
  , _meshes           :: Map n Mesh
    -- |Available models.
  , _models           :: Map n Model
    -- |Available lights.
  , _lights           :: Map n Light
    -- |Available unresolved meshes.
  , _unresolvedMeshes :: Map n (UnresolvedMesh n)
  }

makeLenses ''Available

-- TODO resolution integration should not be (.~), but some kind
-- of union.
-- |Resolve unresolved dependencies.
resolve :: Available n -> Available n
resolve a = a & meshes .~ resolvedMeshes
  where
    resolvedMeshes = mapMaybe (flip resolveMesh vfs) (a^.unresolvedMeshes)
    vfs            = a^.vertexFormats

-- |This typeclass provides 'register', a function used to register
-- resources from a resource-capable monad. The @Proxy n@ is used to
-- select a specific type of resource to load. You can then deduce this:
--
-- @ let loadMesh = load (Proxy :: Mesh) @
--
-- It also exposes the 'resource' function, used to acquire loaded
-- resources.
class Resource a where
  register :: (Ord n) => n -> a -> Available n -> Available n
  resource :: (Ord n) => n -> Available n -> Maybe a

instance Resource VertexFormat where
  register n v = vertexFormats . at n .~ Just v
  resource n = view (vertexFormats . at n)

instance Resource Mesh where
  register n m = meshes . at n .~ Just m
  resource n = view (meshes . at n)

instance Resource Model where
  register n m = models . at n .~ Just m
  resource n = view (models . at n)

instance Resource Light where
  register n l = lights . at n .~ Just l
  resource n = view (lights . at n)
