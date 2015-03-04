{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Quaazar.Core.Loader (
    -- * Loading resources
    Load(..)
  , load
  , loadJSON
  ) where

import Control.Exception ( IOException, catch )
import Control.Monad.Error.Class ( MonadError(..) )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.Aeson
import Data.ByteString.Lazy as B ( readFile )
import Data.Either.Combinators ( eitherToError, mapLeft )
import Quaazar.Utils.Log
import Quaazar.Utils.TimePoint
import System.FilePath

class (FromJSON a) => Load a where
  loadRoot :: a -> String
  loadExt :: a -> String

load :: forall m a. (MonadIO m,MonadLogger m,MonadError Log m,FromJSON a,Load a)
     => FilePath
     -> FilePath
     -> m a
load rootPath n = loadJSON rootPath $ resRoot </> n <.> resExt
  where
    resRoot = loadRoot (undefined :: a) -- TODO: use Proxy instead?
    resExt = loadExt (undefined :: a) -- TODO: use Proxy instead?

loadJSON :: (MonadIO m,MonadLogger m,MonadError Log m,FromJSON a)
         => FilePath
         -> FilePath
         -> m a
loadJSON rootPath path = do
    deb CoreLog $ "parsing '" ++ path ++ "'"
    (st,r,et) <- liftIO $ do
      st <- timePoint
      !r <- (catch (fmap eitherDecode . B.readFile $ rootPath </> path) onError)
      et <- timePoint
      return (st,r,et)
    deb CoreLog $ "parsing time: " ++ show (et - st)
    eitherToError $ mapLeft (Log ErrorLog CoreLog) r
  where
    onError ioe =
      return . Left $ "unable to open file: " ++ show (ioe :: IOException)
