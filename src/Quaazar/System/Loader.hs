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

module Quaazar.System.Loader (
    -- * Loading resources
    Load(..)
  , load_
  , eload_
  , loadJSON
  , rootPath
  ) where

import Control.Exception ( IOException, catch )
import Control.Monad.Error.Class ( MonadError(..) )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.Aeson
import Data.ByteString.Lazy as B ( readFile )
import Data.Either.Combinators ( eitherToError, mapLeft )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped ( MonadScoped )
import Quaazar.Utils.TimePoint
import System.FilePath

--------------------------------------------------------------------------------
-- Loading
class Load opts a | a -> opts where
  -- |Root directory for the given 'a' resource.
  loadRoot :: a -> String
  -- |Extension found at the end of the file hosting the 'a' resource.
  loadExt :: a -> String
  -- |@load name opts@ loads the resource 'name' by looking in the 'data'
  -- tree. Then, @data/subroot/*.ext@ is the common search tree where 'subroot
  -- refers to 'loadRoot' and 'ext' refers to 'loadExt'.
  --
  -- You may pass optional loading arguments via 'opts'.
  --
  -- Note that a default implementation exists for @(FromJSON a) => a@ and
  -- 'opts' == '()'.
  load :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m)
       => FilePath
       -> opts
       -> m a
  default load :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m,FromJSON a)
               => FilePath
               -> ()
               -> m a
  load n _ = loadJSON rootPath $ resRoot </> n <.> resExt
    where
      resRoot = loadRoot (undefined :: a)
      resExt = loadExt (undefined :: a)
  -- |@eload path opts@ explicitely loads a resource at the given path.
  --
  -- Note that a default implementation exists for @(FromJSON a) => a@ and
  -- 'opts' == '()'.
  eload :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m)
        => FilePath
        -> opts
        -> m a
  default eload :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m,FromJSON a)
                => FilePath
                -> ()
                -> m a
  eload path _ = loadJSON "" path

-- |Resources root path.
rootPath :: FilePath
rootPath = "./data"

-- |Load without optional arguments.
load_ :: (Load () a,MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m)
      => FilePath
      -> m a
load_ n = load n ()

-- |Explicit load without optional arguments.
eload_ :: (Load () a,MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m)
       => FilePath
       -> m a
eload_ path = eload path ()

--------------------------------------------------------------------------------
-- Helpers

-- |Load a JSON value – this is, @(FromJSON a) => a@.
loadJSON :: (MonadIO m,MonadLogger m,MonadError Log m,FromJSON a)
         => FilePath
         -> FilePath
         -> m a
loadJSON root path = do
    deb CoreLog $ "parsing '" ++ path ++ "'"
    (st,r,et) <- liftIO $ do
      st <- timePoint
      !r <- (catch (fmap eitherDecode . B.readFile $ root </> path) onError)
      et <- timePoint
      return (st,r,et)
    deb CoreLog $ "parsing time: " ++ show (et - st)
    eitherToError $ mapLeft (Log ErrorLog CoreLog) r
  where
    onError ioe =
      return . Left $ "unable to open file: " ++ show (ioe :: IOException)
