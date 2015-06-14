{-# LANGUAGE RankNTypes #-}

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

module Quaazar.System.Resource (
    -- * Resource map & manager
    ResourceMap(..)
  , getResourceMap
  , getSimpleManager
    -- * Re-exported
  , module Control.Monad.Error.Class
  , module Control.Monad.Trans
  , module Quaazar.System.Loader
  , module Quaazar.Utils.Log
  , module Quaazar.Utils.Scoped
  ) where

import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Data.IORef
import Data.Map as M ( Map, empty )
import Quaazar.System.Loader
import Quaazar.Utils.Log ( Log, MonadLogger )
import Quaazar.Utils.Scoped ( MonadScoped )

data ResourceMap a = ResourceMap {
    insertRes :: forall m. (MonadIO m) => String -> a -> m ()
  , lookupRes :: forall m. (MonadIO m) => String -> m (Maybe a)
  }

getResourceMap :: (MonadIO m) =>  m (ResourceMap a)
getResourceMap = do
    ref <- liftIO $ newIORef empty
    pure $ ResourceMap (inject_ ref) (retrieve_ ref)
  where
    inject_ ref name r = liftIO . modifyIORef ref $ at name .~ Just r
    retrieve_ ref name = liftIO . fmap (view $ at name) $ readIORef ref

getSimpleManager :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m,Load () a)
                 => m (String -> m a)
getSimpleManager = do
  resMap <- getResourceMap
  pure $ \name -> lookupRes resMap name >>= maybe (load_ name) pure
