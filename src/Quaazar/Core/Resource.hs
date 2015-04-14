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
    Manager(Manager)
  , Resource(..)
    -- * Retrieving and releasing resources
  , retrieve
  , release
    -- * Miscellaneous
  , lookupInsert
  ) where

import Control.Applicative ( Applicative )
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Data.IORef ( modifyIORef, newIORef, readIORef, writeIORef )
import Data.Map as M ( Map, delete, empty, insert, lookup )
import Quaazar.Core.Loader ( Load(load) )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

data Manager dep r = Manager {
    retrieve :: (Applicative m,MonadIO m,MonadScoped IO m,MonadError Log m,MonadLogger m) => dep -> String -> m r
  , release :: (MonadIO m) => String -> m ()
  }

class Resource dep r | r -> dep where
  manager :: (MonadIO m) => FilePath -> m (Manager dep r)
  default manager :: (MonadIO m,Load () r) => FilePath -> m (Manager () r)
  manager root = do
      ref <- liftIO $ newIORef empty
      return $ Manager (retrieve_ ref) (release_ ref)
    where
      retrieve_ ref _ name = do
        mp <- liftIO $ readIORef ref
        (mp',r) <- lookupInsert root () mp name
        liftIO $ writeIORef ref mp'
        return r
      release_ ref name = liftIO . modifyIORef ref $ delete name

lookupInsert :: (MonadIO m,MonadScoped IO m,MonadError Log m,MonadLogger m,Load opts r)
             => FilePath
             -> opts
             -> Map String r
             -> String
             -> m (Map String r,r)
lookupInsert root opts mp key = case M.lookup key mp of
  Just res -> return (mp,res) -- resource already in cache 
  Nothing -> do -- resource not in cache; load it
    res <- load root key opts
    return (insert key res mp,res)
