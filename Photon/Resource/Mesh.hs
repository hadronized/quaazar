-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- This module exports anything you might need to deal with mesh resources.
----------------------------------------------------------------------------

module Photon.Resource.Mesh (
    -- * Loading meshes
    loadMesh
  ) where

import Control.Monad.Except ( MonadError, throwError )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.Map as M ( Map )
import Photon.Core.Vertex ( VertexFormat )
import Photon.Core.Mesh ( Mesh, meshParser )
import Photon.Utils.Parsing

loadMesh :: (MonadError String m,MonadIO m)
         => Map String VertexFormat
         -> FilePath
         -> m Mesh
loadMesh vformats path = do
    f <- liftIO (readFile path)
    either (throwError . show) return (parsed f)
  where
    parsed = runParser (meshParser vformats) [] path
