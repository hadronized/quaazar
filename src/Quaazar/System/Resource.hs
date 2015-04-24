-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Quaazar.System.Resource where

import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) ) 
import Control.Monad.Trans.State ( StateT, get, modify, put )
import Data.Map as M ( Map, empty )
import Numeric.Natural ( Natural )
import Quaazar.Geometry.Mesh ( Mesh )
import Quaazar.Lighting.Light ( Omni )
import Quaazar.Render.GL.Texture ( CompareFunc, Filter, Texture2D, Wrap )
import Quaazar.Render.Mesh ( GPUMesh, gpuMesh )
import Quaazar.System.Loader ( load, load_ )
import Quaazar.Technics.Lighting.Phong ( PhongMaterial(..)
                                       , PhongMaterialManifest(..) )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

--------------------------------------------------------------------------------
-- Cache
data Cache = Cache {
    _cachedMeshes         :: Map String Mesh
  , _cachedGPUMeshes      :: Map String GPUMesh
  , _cachedTexture2Ds     :: Map String Texture2D
  , _cachedOmnis          :: Map String Omni
  , _cachedPhongMaterials :: Map String PhongMaterial
  }

makeLenses ''Cache

class (Monad m) => HasCache m where
  getCache    :: m Cache
  modifyCache :: (Cache -> Cache) -> m ()
  putCache    :: Cache -> m ()

instance (Monad m) => HasCache (StateT Cache m) where
  getCache = get
  modifyCache = modify
  putCache = put

emptyCache :: Cache
emptyCache = Cache empty empty empty empty empty

--------------------------------------------------------------------------------
-- Resource
class Resource r where
  type Opt r :: *
  retrieve :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m,HasCache m)
           => String
           -> Opt r 
           -> m r

instance Resource Mesh where
  type Opt Mesh = ()
  retrieve name _ = do
    found <- fmap (view $ cachedMeshes . at name) getCache
    case found of
      Just m -> pure m
      Nothing -> do
        loaded <- load_ name
        modifyCache $ cachedMeshes . at name .~ Just loaded
        pure loaded

instance Resource GPUMesh where
  type Opt GPUMesh = ()
  retrieve name _ = do
    found <- fmap (view $ cachedGPUMeshes . at name) getCache
    case found of
      Just m -> pure m
      Nothing -> do
        gmsh <- retrieve name () >>= gpuMesh
        modifyCache $ cachedGPUMeshes . at name .~ Just gmsh
        pure gmsh

instance Resource Texture2D where
  type Opt Texture2D = (Wrap,Filter,Maybe CompareFunc,Natural,Natural)
  retrieve name opt = do
    found <- fmap (view $ cachedTexture2Ds . at name) getCache
    case found of
      Just t -> pure t
      Nothing -> do
        loaded <- load name opt
        modifyCache $ cachedTexture2Ds . at name .~ Just loaded
        pure loaded

instance Resource Omni where
  type Opt Omni = ()
  retrieve name _ = do
    found <- fmap (view $ cachedOmnis . at name) getCache
    case found of
      Just o -> pure o
      Nothing -> do
        loaded <- load_ name 
        modifyCache $ cachedOmnis . at name .~ Just loaded
        pure loaded

instance Resource PhongMaterial where
  type Opt PhongMaterial = (Wrap,Filter,Maybe CompareFunc,Natural,Natural)
  retrieve name opt = do
    found <- fmap (view $ cachedPhongMaterials . at name) getCache
    case found of
      Just o -> pure o
      Nothing -> do
        PhongMaterialManifest diff spec gloss <- load_ name
        diffTex <- retrieve diff opt
        specTex <- retrieve spec opt
        glossTex <- retrieve gloss opt
        let mat = PhongMaterial diffTex specTex glossTex
        modifyCache $ cachedPhongMaterials . at name .~ Just mat 
        pure mat

