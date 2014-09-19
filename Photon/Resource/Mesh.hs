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

newtype UnresolvedMesh n = UnresolvedMesh { resolveMesh :: Map n VertexFormat -> Maybe Mesh }

instance (Ord n,FromJSON n) => FromJSON (UnresolvedMesh n) where
  parseJSON = withObject "mesh" $ \o -> do
      verts <- o .: "vertices"
      vgr   <- o .: "vgroup"
      return . UnresolvedMesh $ mapFun verts vgr
    where
      mapFun verts vgr m = Mesh <$> resolveVertices verts m <*> pure vgr

newtype UnresolvedVertices n = UnresolvedVertices { resolveVertices :: Map n VertexFormat -> Maybe Vertices }

instance (Ord n,FromJSON n) => FromJSON (UnresolvedVertices n) where
  parseJSON = withObject "vertices" $ \o -> do
      format <- o .: "format"
      verts  <- o .: "vertices"
      return . UnresolvedVertices $ mapFun format verts
    where
      mapFun format verts m = Vertices <$> M.lookup format m <*> pure verts
