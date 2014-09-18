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
  , vertexFormats
  , meshes
  , models
  , lights
    -- * Acquiring resources
  , Resource(..)
  ) where

import Control.Lens ( makeLenses )
import Control.Monad.State ( MonadState )
import Control.Monad.Trans ( MonadIO(..) )
import Data.Map as M ( Map )
import Data.Proxy ( Proxy )
import Photon.Core.Light ( Light )
import Photon.Core.Mesh ( Mesh )
import Photon.Core.Model ( Model )
import Photon.Core.Vertex ( VertexFormat )
import Photon.Resource.Mesh ( UnresolvedMesh )

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
  , _unresolvedMeshes :: Map n UnresolvedMesh
  } deriving (Eq,Show)

makeLenses ''Available

-- |This typeclass provides 'load', a function used to load resources
-- from a resource-capable monad. The @Proxy n@ is used to select a
-- specific type of resource to load. You can then deduce this:
--
-- @ let loadMesh = load (Proxy :: Mesh) @
--
-- It also exposes the 'resource' function, used to acquire loaded
-- resources.
class Resource a where
  load     :: (MonadIO m,MonadState Available m) => Proxy a -> n -> m ()
  resource :: (MonadState Available m) => n -> m (Maybe a)

instance Resource VertexFormat where
  load _ n = do
    vf <- liftIO (loadJSON n)
    vertexFormats . at n .= Just vf
  resource n = fmap (preview . at n) (use vertexFormats)
