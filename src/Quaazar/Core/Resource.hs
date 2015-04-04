{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Quaazar.Core.Resource (
    -- * Resource manager
    Manager
  , manager
    -- * Retrieving and releasing resources
  , retrieve
  , release
  ) where

import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Data.IORef ( modifyIORef, newIORef, readIORef, writeIORef )
import Data.Map as M ( Map, delete, empty, insert, lookup )
import Quaazar.Core.Loader ( Load(load) )
import Quaazar.Utils.Log

data Manager deps r = Manager {
    retrieve :: (MonadIO m,MonadError Log m,MonadLogger m,Load r) => deps -> String -> m r
  , release :: (MonadIO m) => String -> m ()
  }

class Resource dep r | r -> dep where
  manager :: (MonadIO m,Load r) => FilePath -> m (Manager dep r)
  default manager :: (MonadIO m,Load r) => FilePath -> m (Manager () r)
  manager root = do
      ref <- liftIO $ newIORef empty
      return $ Manager (retrieve_ ref) (release_ ref)
    where
      retrieve_ ref _ name = do
        mp <- liftIO $ readIORef ref
        (mp',r) <- lookupInsert root mp name
        liftIO $ writeIORef ref mp'
        return r
      release_ ref name = liftIO . modifyIORef ref $ delete name

lookupInsert :: (MonadIO m,MonadError Log m,MonadLogger m,Load r)
             => FilePath
             -> Map String r
             -> String
             -> m (Map String r,r)
lookupInsert root mp key = case M.lookup key mp of
  Just res -> return (mp,res) -- resource already in cache 
  Nothing -> do -- resource not in cache; load it
    res <- load root key
    return (insert key res mp,res)
