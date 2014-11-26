-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Meshes are the way geometry is built up. A 'Mesh' is a bunch of vertices
-- and a description of faces through a vertices group.
----------------------------------------------------------------------------

module Photon.Core.Mesh (
    -- * Mesh
    Mesh(Mesh)
  , meshVertices
  , meshVGroup
  , changeVertices
  , changeVGroup
  , MeshSpawned(..)
  , MeshLost(..)
  , MeshEffect(..)
    -- * Re-exported modules
  , module Photon.Core.Vertex
  , module Photon.Core.VGroup
  ) where

import Control.Applicative
import Control.Lens ( makeLenses )
import Data.Aeson
import Photon.Core.Effect ( Effect(..), EffectfulManage(..), Managed )
import Photon.Core.Vertex
import Photon.Core.VGroup

-- |A mesh is a pair of vertices and vertex group. See 'meshVertices' and
-- 'meshVGroup' for further details.
data Mesh = Mesh {
    _meshVertices :: Vertices
  , _meshVGroup   :: VGroup
  } deriving (Eq,Show)

instance FromJSON Mesh where
  parseJSON = withObject "mesh" $ \o -> Mesh <$> o .: "vertices" <*> o .: "vgroup"

makeLenses ''Mesh

data MeshSpawned = MeshSpawned (Managed Mesh) deriving (Eq,Show)

data MeshLost = MeshLost (Managed Mesh) deriving (Eq,Show)

data MeshEffect
  = ChangedVertices (Managed Mesh) Vertices
  | ChangedVGroup (Managed Mesh) VGroup
    deriving (Eq,Show)

instance EffectfulManage Mesh MeshSpawned MeshLost where
  spawned = MeshSpawned
  lost = MeshLost

changeVertices :: (Effect MeshEffect m)
               => Managed Mesh
               -> (Vertices -> Vertices)
               -> m (Managed Mesh)
changeVertices msh f = do
    react (ChangedVertices msh newVerts)
    return (msh . meshVertices .~ newVerts)
  where newVerts = f (msh^.meshVertices)

changeVGroup :: (Effect MeshEffect m)
             => Managed Mesh
             -> (VGroup -> VGroup)
             -> m (Managed Mesh)
changeVGroup msh f = do
    react (ChangedVGroup msh newVGroup)
    return (mesh . meshVGroup .~ newVGroup)
  where newVGroup = f (msh^.meshVGroup)
