-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Vertex formats can be considered as resources. This module exports
-- anything you might need to deal with such resources.
----------------------------------------------------------------------------

module Photon.Resource.VertexFormat (
    -- * Loading vertex formats
    loadVertexFormat
  ) where

import Control.Monad.Trans ( MonadIO, liftIO )
import Control.Monad.Except ( MonadError, throwError )
import Data.Aeson ( eitherDecode )
import Data.ByteString.Lazy as B ( readFile )
import Photon.Core.Vertex ( VertexFormat )

loadVertexFormat :: (MonadError String m,MonadIO m) => FilePath -> m VertexFormat
loadVertexFormat path = liftIO (B.readFile path) >>= eitherDecode_
  where
    eitherDecode_ = either throwError return . eitherDecode
