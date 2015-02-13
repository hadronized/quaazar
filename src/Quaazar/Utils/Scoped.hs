{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Quaazar.Utils.Scoped (
    -- * Scoped monad
    MonadScoped(..)
    -- * IO scoped monad transformer
  , IOScopedT
  , runIOScopedT
  ) where

import Control.Applicative ( Applicative )
import Control.Monad.Base ( MonadBase(..) )
import Control.Monad.Trans ( MonadIO(..) )
import Control.Monad.Trans.State ( StateT, modify, runStateT )

class (MonadBase b m) => MonadScoped b m where
  scoped :: b () -> m ()

newtype IOScopedT m a = IOScopedT { unIOScopedT :: StateT (IO ()) m a } deriving (Applicative,Functor,Monad)

deriving instance (MonadBase IO m) => MonadBase IO (IOScopedT m)

instance (MonadBase IO m) => MonadScoped IO (IOScopedT m) where
  scoped a = IOScopedT $ modify (>>a)

runIOScopedT :: (MonadIO m) => IOScopedT m a -> m a
runIOScopedT scope = do
  (a,cleanup) <- runStateT (unIOScopedT scope) (return ())
  liftIO cleanup
  return a
