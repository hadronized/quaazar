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
import Control.Monad.Error.Class ( MonadError(..) )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.ByteString.Lazy as B ( readFile )
import Data.Proxy ( Proxy )
import Data.Aeson
import Photon.Core.Light ( Light )
import Photon.Core.Mesh ( Mesh )
import Photon.Core.Model ( Model )
import Photon.Core.Vertex ( VertexFormat )
import Photon.Resource.Available
import Photon.Utils.Log
import System.FilePath

loadJSON :: (MonadError String m,MonadIO m,FromJSON a)
         => FilePath
         -> m a
loadJSON path = liftIO (B.readFile path) >>= eitherDecode_
  where
    eitherDecode_ = either throwError return . eitherDecode

--  load     :: (MonadIO m,MonadLogger m) => Proxy a -> n -> Available n -> m (Available n)

