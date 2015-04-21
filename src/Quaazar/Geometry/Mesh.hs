{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Meshes are the way geometry is built up. A 'Mesh' is a bunch of
-- 'Vertices' and a description of faces through a 'VGroup'.
----------------------------------------------------------------------------

module Quaazar.Geometry.Mesh (
    -- * Mesh
    Mesh(Mesh)
  , meshVertices
  , meshVGroup
    -- * Resource
  , MeshManager
    -- * Re-exported modules
  , module Quaazar.Geometry.Vertex
  , module Quaazar.Geometry.VGroup
  ) where

import Control.Lens
import Data.Aeson
import Quaazar.Geometry.Vertex
import Quaazar.Geometry.VGroup
import Quaazar.System.Loader ( Load(..) )
import Quaazar.System.Resource ( Manager, Resource )

-- |A mesh is a pair of vertices and vertex group.
data Mesh = Mesh {
    _meshVertices :: Vertices
  , _meshVGroup   :: VGroup
  } deriving (Eq,Show)

makeLenses ''Mesh

instance FromJSON Mesh where
  parseJSON = withObject "mesh" $ \o -> Mesh <$> o .: "vertices" <*> o .: "vgroup"

instance Load () Mesh where
  loadRoot = const "meshes"
  loadExt = const "qmsh"

instance Resource () Mesh

-- |Mesh manager.
type MeshManager = Manager () Mesh
