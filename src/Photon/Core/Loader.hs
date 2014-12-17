{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Photon.Core.Loader (
    -- * Loading resources
    loadJSON
  , Load
  , load 
    -- * Loaders
  , loadMesh
  , loadMaterial
  , loadLight
  ) where

import Control.Exception ( IOException, catch )
import Control.Monad ( MonadPlus(..) )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.ByteString.Lazy as B ( readFile )
import Data.Aeson
import Photon.Core.Light ( Light )
import Photon.Core.Material ( Material )
import Photon.Core.Mesh ( Mesh )
import Photon.Utils.Log
import Photon.Utils.TimePoint
import System.FilePath

rootPath :: FilePath
rootPath = "data"

class (FromJSON a) => Load a where
  load :: (MonadIO m,MonadLogger m,MonadPlus m)
       => String
       -> m a

instance Load Mesh where
  load = loadMesh

instance Load Material where
  load = loadMaterial

instance Load Light where
  load = loadLight

loadJSON :: (MonadIO m,MonadLogger m,FromJSON a) => FilePath -> m (Either String a)
loadJSON path = do
    deb CoreLog $ "parsing '" ++ path ++ "'"
    (st,r,et) <- liftIO $ do
      st <- timePoint
      !r <- (catch (fmap eitherDecode . B.readFile $ rootPath </> path) onError)
      et <- timePoint
      return (st,r,et)
    deb CoreLog $ "parsing time: " ++ show (et - st)
    return r
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
      info CoreLog $ "loaded mesh '" ++ n ++ "'"
      return msh

loadMaterial :: (MonadIO m,MonadLogger m,MonadPlus m) => String -> m Material
loadMaterial n = loadJSON path >>= either loadError ok 
  where
    path = "materials" </> n <.> "ymat"
    loadError e = do
      err CoreLog $ "failed to load material '" ++ path ++ "': " ++ e
      mzero
    ok mat = do
      info CoreLog $ "loaded material '" ++ n ++ "'"
      return mat

loadLight :: (MonadIO m,MonadLogger m,MonadPlus m) => String -> m Light
loadLight n = loadJSON path >>= either loadError ok 
  where
    path = "lights" </> n <.> "ylig"
    loadError e = do
      err CoreLog $ "failed to load light '" ++ path ++ "': " ++ e
      mzero
    ok lig = do
      info CoreLog $ "loaded light '" ++ n ++ "'"
      return lig
