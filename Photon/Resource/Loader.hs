{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- This module exports the resource loader.
----------------------------------------------------------------------------

module Photon.Resource.Loader where

import Control.Lens hiding ( (<.>) )
import Control.Monad ( liftM )
import Control.Monad.Error.Class ( MonadError(..) )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.ByteString.Lazy as B ( readFile )
import Data.Proxy ( Proxy(..) )
import Data.Aeson
import Photon.Core.Light ( Light )
import Photon.Core.Mesh ( Mesh )
import Photon.Core.Model ( Model )
import Photon.Core.Vertex ( VertexFormat )
import Photon.Resource.Available
import Photon.Utils.Log
import System.FilePath

loadJSON :: (FromJSON a,MonadIO m) => FilePath -> m (Either String a)
loadJSON path =  liftM eitherDecode (liftIO $ B.readFile path)

class Load a n where
  resourceFilePath :: Proxy a -> n -> FilePath

instance Load VertexFormat String where
  resourceFilePath _ n = "vformats" </> n <.> "yvft"

loadVertexFormat :: (Ord n,Load VertexFormat n,MonadIO m,MonadLogger m)
                 => n
                 -> Available n
                 -> m (Available n)
loadVertexFormat n available = loadJSON path >>= register
  where
    path        = resourceFilePath (Proxy :: Proxy VertexFormat) n
    register    = either loadError (\vf -> return $ available & vertexFormats . at n .~ Just vf)
    loadError e = do
      err CoreLog $ "failed to load vertex format '" ++ path ++ "': " ++ e
      return available
