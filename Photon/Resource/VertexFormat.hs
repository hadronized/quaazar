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
    -- *
  ) where

import Control.Monad.Trans ( MonadIO, liftIO )
import Control.Monad.Except ( MonadError, throwError )
import Photon.Core.Vertex ( VertexFormat, vertexFormatParser )
import Photon.Utils.Log ( MonadLogger )
import Text.Parsec.String ( parseFromFile )

loadVertexFormat :: (MonadLogger m,MonadError String m,MonadIO m)
                 => FilePath
                 -> m VertexFormat
loadVertexFormat path = do
    vf <- liftIO $ parseFromFile vertexFormatParser path
    either (throwError . show) return vf
