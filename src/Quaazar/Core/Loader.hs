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

module Quaazar.Core.Loader (
    -- * Loading resources
    Load(..)
  , load
  , loadJSON
  , rootPath
  ) where

import Control.Exception ( IOException, catch )
import Control.Monad.Error.Class ( MonadError(..) )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.Aeson
import Data.ByteString.Lazy as B ( readFile )
import Data.Either.Combinators ( mapLeft )
import Quaazar.Utils.Either ( generalizeEither )
import Quaazar.Utils.Log
import Quaazar.Utils.TimePoint
import System.FilePath

rootPath :: FilePath
rootPath = "data"

class (FromJSON a) => Load a where
  loadRoot :: a -> String
  loadExt :: a -> String

load :: forall m a. (MonadIO m,MonadLogger m,MonadError Log m,FromJSON a,Load a)
     => FilePath
     -> m a
load n = loadJSON $ root </> n <.> ext
  where
    root = loadRoot (undefined :: a) -- TODO: use Proxy instead?
    ext = loadExt (undefined :: a) -- TODO: use Proxy instead?

loadJSON :: (MonadIO m,MonadLogger m,MonadError Log m,FromJSON a)
         => FilePath
         -> m a
loadJSON path = do
    deb CoreLog $ "parsing '" ++ path ++ "'"
    (st,r,et) <- liftIO $ do
      st <- timePoint
      !r <- (catch (fmap eitherDecode . B.readFile $ rootPath </> path) onError)
      et <- timePoint
      return (st,r,et)
    deb CoreLog $ "parsing time: " ++ show (et - st)
    generalizeEither $ mapLeft (Log ErrorLog CoreLog) r
  where
    onError ioe =
      return . Left $ "unable to open file: " ++ show (ioe :: IOException)
