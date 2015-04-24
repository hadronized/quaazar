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

module Quaazar.System.Resource (
  ) where

import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) ) 
import Control.Monad.Trans.State ( StateT, get, modify, put )
import Data.Map as M ( Map, empty, insert, lookup )
import Quaazar.Geometry.Mesh ( Mesh )
import Quaazar.Render.GL.Texture ( Texture2D )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

--------------------------------------------------------------------------------
-- Cache
data Cache = Cache {
    _cachedMeshes :: Map String Mesh
  , _cachedTexture2Ds :: Map String Texture2D
  } deriving (Eq,Show)

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
emptyCache = Cache empty empty

--------------------------------------------------------------------------------
-- Resource
class Resource r where
  type Opt r :: *
  retrieve :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m,HasCache m)
           => String
           -> Opt r 
           -> m r

{-
instance Resource Mesh where
  type Opt Mesh = ()
  retrieve name _ = do
    found <- fmap (M.lookup name . view cachedMeshes) getCache
    case found of
      Just m -> pure m
      Nothing -> do
        loaded <- fmap Mesh $ liftIO getLine
        modifyCache $ cachedMeshes . at name .~ Just loaded
        pure loaded
-}
{-
instance Resource Texture where
  type Opt Texture = ()
  retrieve name _ = do
    found <- fmap (M.lookup name . view cachedTextures) getCache
    case found of
      Just m -> pure m
      Nothing -> do
        loaded <- fmap Texture $ liftIO getLine
        modifyCache $ cachedTextures . at name .~ Just loaded
        pure loaded
-}
