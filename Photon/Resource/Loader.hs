{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Photon.Resource.Loader (
    -- * Resource loaders
    loadMesh
  , loadMaterial
  , loadLight
    -- * More complex loaders
  , loadLights
  , loadMeshes
  , loadObjectsPerMaterial
  , loadObjects
  , loadSceneRel
  ) where

import Control.Applicative
import Control.Lens hiding ( (<.>) )
import Control.Monad ( liftM )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.ByteString.Lazy as B ( readFile )
import Data.Aeson
import Photon.Resource.Available
import Photon.Utils.Log
import System.FilePath

loadJSON :: (FromJSON a,MonadIO m) => FilePath -> m (Either String a)
loadJSON path = liftM eitherDecode (liftIO $ B.readFile path)

loadMesh :: (MonadIO m,MonadLogger m)
         => String
         -> Available
         -> m Available
loadMesh n available = loadJSON path >>= register
  where
    path        = "meshes" </> n <.> "ymsh"
    register    = either loadError (\m -> return $ available & meshes . at n .~ Just m)
    loadError e = do
      err CoreLog $ "failed to load mesh '" ++ path ++ "': " ++ e
      return available

loadMaterial :: (MonadIO m,MonadLogger m)
             => String
             -> Available
             -> m Available
loadMaterial n available = loadJSON path >>= register
  where
    path        = "materials" </> n <.> "ymdl"
    register    = either loadError (\m -> return $ available & materials . at n .~ Just m)
    loadError e = do
      err CoreLog $ "failed to load material '" ++ path ++ "': " ++ e
      return available

loadLight :: (MonadIO m,MonadLogger m)
          => String
          -> Available
          -> m Available
loadLight n available = loadJSON path >>= register
  where
    path        = "lights" </> n <.> "ylig"
    register    = either loadError (\l -> return $ available & lights . at n .~ Just l)
    loadError e = do
      err CoreLog $ "failed to load light '" ++ path ++ "': " ++ e
      return available

loadLights :: (MonadIO m,MonadLogger m,FromJSON a) => [a] -> m [(a,Light)]
loadLights = fmap <$> zip <*> mapM loadLight

loadMeshes :: (MonadIO m,MonadLogger m,FromJSON a) => [a] -> m [(a,Mesh)]
loadMeshes = fmap <$> zip <*> mapM loadMesh

loadObjectsPerMaterial :: (MonadIO m,MonadLogger m,FromJSON a) => a -> [a] -> n (Material,[(a,Mesh)])
loadObjectsPerMaterial mat objs = (,) <$> loadMaterial mat <*> loadMeshes objs

loadObjects :: (MonadIO m,MonadLogger m,FromJSON a) => [(a,[a])] -> n [(Material [(a,Mesh)])]
loadObjects = mapM (curry loadObjectsPerMaterial)

loadSceneRel :: (MonadIO m,MonadLogger m,FromJSON a) => [a] -> [(a,[a])] -> Projection -> n SceneRel
loadSceneRel ligs objs proj = SceneRel proj <$> loadLights ligs <*> loadObjects objs
