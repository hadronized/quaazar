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
import Control.Exception ( IOException, catch )
import Control.Monad ( MonadPlus(..) )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.ByteString.Lazy as B ( readFile )
import Data.Aeson
import Photon.Core.Light ( Light )
import Photon.Core.Material ( Material )
import Photon.Core.Mesh ( Mesh )
import Photon.Core.Projection ( Projection )
import Photon.Core.Scene ( SceneRel(..) )
import Photon.Utils.Log
import System.FilePath

rootPath :: FilePath
rootPath = "data"

loadJSON :: (MonadIO m,FromJSON a) => FilePath -> m (Either String a)
loadJSON path = liftIO $ catch (fmap eitherDecode . B.readFile $ rootPath </> path) onError
  where
    onError ioe = return . Left $ "unable to open file: " ++ show (ioe :: IOException)

loadMesh :: (MonadIO m,MonadLogger m,MonadPlus m) => String -> m Mesh
loadMesh n = loadJSON path >>= either loadError ok 
  where
    path = "meshes" </> n <.> "ymsh"
    loadError e = do
      err CoreLog $ "failed to load mesh '" ++ path ++ "': " ++ e
      mzero
    ok msh = do
      info CoreLog $ "loaded mesh '" ++ path ++ "'"
      return msh

loadMaterial :: (MonadIO m,MonadLogger m,MonadPlus m) => String -> m Material
loadMaterial n = loadJSON path >>= either loadError ok 
  where
    path = "materials" </> n <.> "ymat"
    loadError e = do
      err CoreLog $ "failed to load material '" ++ path ++ "': " ++ e
      mzero
    ok mat = do
      info CoreLog $ "loaded material '" ++ path ++ "'"
      return mat

loadLight :: (MonadIO m,MonadLogger m,MonadPlus m) => String -> m Light
loadLight n = loadJSON path >>= either loadError ok 
  where
    path = "lights" </> n <.> "ylig"
    loadError e = do
      err CoreLog $ "failed to load light '" ++ path ++ "': " ++ e
      mzero
    ok lig = do
      info CoreLog $ "loaded light '" ++ path ++ "'"
      return lig

-- FIXME: GHC 7.10 Applicative-Monad proposal
loadLights :: (Applicative m,MonadIO m,MonadLogger m,MonadPlus m) => [String] -> m [(String,Light)]
loadLights = fmap <$> zip <*> mapM loadLight

loadMeshes :: (Applicative m,MonadIO m,MonadLogger m,MonadPlus m) => [String] -> m [(String,Mesh)]
loadMeshes = fmap <$> zip <*> mapM loadMesh

loadObjectsPerMaterial :: (Applicative m,MonadIO m,MonadLogger m,MonadPlus m) => String -> [String] -> m (Material,[(String,Mesh)])
loadObjectsPerMaterial mat objs = (,) <$> loadMaterial mat <*> loadMeshes objs

loadObjects :: (Applicative m,MonadIO m,MonadLogger m,MonadPlus m) => [(String,[String])] -> m [(Material,[(String,Mesh)])]
loadObjects = mapM (uncurry loadObjectsPerMaterial)

loadSceneRel :: (Applicative m,MonadIO m,MonadLogger m,MonadPlus m) => [String] -> [(String,[String])] -> Projection -> m (SceneRel String)
loadSceneRel ligs objs proj = SceneRel proj <$> loadLights ligs <*> loadObjects objs
