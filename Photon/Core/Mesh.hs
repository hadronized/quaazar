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
--
-- This module also exports all the required parsers needed to parse a mesh
-- and its components.
----------------------------------------------------------------------------

module Photon.Core.Mesh (
    -- * Mesh
    Mesh(Mesh)
  , meshVertices
  , meshVGroup
    -- * Re-exported modules
  , module Photon.Core.Vertex
  , module Photon.Core.VGroup
  ) where

import Control.Lens ( makeLenses )
import Photon.Core.Vertex
import Photon.Core.VGroup

-- |A mesh is a pair of vertices and vertex group. See 'meshVertices' and
-- 'meshVGroup' for further details.
data Mesh = Mesh {
    _meshVertices :: Vertices
  , _meshVGroup   :: VGroup
  } deriving (Eq,Show)

makeLenses ''Mesh
