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
-- Meshes are the way geometry is built up. A 'Mesh' is a bunch of vertices
-- and a description of faces through a vertices group.
----------------------------------------------------------------------------

module Photon.Core.Mesh (
    -- * Mesh
    Mesh(Mesh)
  , meshVertices
  , meshVGroup
    -- * Reaction
  , MeshSpawned(..)
  , MeshLost(..)
  , MeshEffect(..)
  , changeVertices
  , changeVGroup
  , useMaterial
  , renderMesh
    -- * Re-exported modules
  , module Photon.Core.Vertex
  , module Photon.Core.VGroup
  ) where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Photon.Core.Effect
import Photon.Core.Material ( Material )
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

data MeshSpawned = MeshSpawned (Managed Mesh)

data MeshLost = MeshLost (Managed Mesh)

data MeshEffect
  = VerticesChanged (Managed Mesh) Vertices
  | VGroupChanged (Managed Mesh) VGroup
  | UseMaterial (Managed Mesh) (Managed Material)
  | RenderMesh (Managed Mesh)

instance EffectfulManage Mesh MeshSpawned MeshLost where
  spawned = MeshSpawned
  lost = MeshLost

changeVertices :: (Effect MeshEffect m)
               => Managed Mesh
               -> (Vertices -> Vertices)
               -> m (Managed Mesh)
changeVertices msh f = do
    react (VerticesChanged msh newVerts)
    return (msh & managed . meshVertices .~ newVerts)
  where newVerts = f (msh^.managed.meshVertices)

changeVGroup :: (Effect MeshEffect m)
             => Managed Mesh
             -> (VGroup -> VGroup)
             -> m (Managed Mesh)
changeVGroup msh f = do
    react (VGroupChanged msh newVGroup)
    return (msh & managed . meshVGroup .~ newVGroup)
  where newVGroup = f (msh^.managed.meshVGroup)

useMaterial :: (Effect MeshEffect m)
            => Managed Mesh
            -> Managed Material
            -> m ()
useMaterial msh mat = react (UseMaterial msh mat)

renderMesh :: (Effect MeshEffect m)
           => Managed Mesh
           -> m ()
renderMesh = react . RenderMesh
