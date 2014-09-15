-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- This module exports a JSON loader for lights.
----------------------------------------------------------------------------

module Photon.Resource.Loader (
    -- * JSON loader
    loadJSON
  ) where

import Control.Monad.Error.Class ( MonadError(..) )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.ByteString.Lazy as B ( readFile )
import Data.Aeson

loadJSON :: (MonadError String m,MonadIO m,FromJSON a)
         => FilePath
         -> m a
loadJSON path = liftIO (B.readFile path) >>= eitherDecode_
  where
    eitherDecode_ = either throwError return . eitherDecode
