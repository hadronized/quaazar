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
-- Meshes are resources too. But they’re special resources. Since they
-- depend on a few other objects, this module exposes convenient types for
-- deferred dependencies resolving.
----------------------------------------------------------------------------

module Photon.Resource.Mesh where

import Control.Applicative
import Data.Map as M ( Map, lookup )
import Data.Aeson
import Photon.Core.Mesh ( Mesh(..) )
import Photon.Core.Vertex ( VertexFormat, Vertices(Vertices) )

newtype UnresolvedMesh = UnresolvedMesh { resolveMesh :: Map String VertexFormat -> Maybe Mesh }

instance FromJSON UnresolvedMesh where
  parseJSON = withObject "mesh" $ \o -> do
      verts <- o .: "vertices"
      vgr   <- o .: "vgroup"
      return . UnresolvedMesh $ mapFun verts vgr
    where
      mapFun verts vgr m = Mesh <$> resolveVertices verts m <*> pure vgr

newtype UnresolvedVertices = UnresolvedVertices { resolveVertices :: Map String VertexFormat -> Maybe Vertices }

instance FromJSON UnresolvedVertices where
  parseJSON = withObject "vertices" $ \o -> do
      format <- o .: "format"
      verts  <- o .: "vertices"
      return . UnresolvedVertices $ mapFun format verts
    where
      mapFun format verts m = Vertices <$> M.lookup format m <*> pure verts
