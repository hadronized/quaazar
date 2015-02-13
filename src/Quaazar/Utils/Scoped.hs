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

module Quaazar.Utils.Scoped where

import Control.Monad.Base ( MonadBase(..) )
import Control.Monad.Trans ( MonadIO(..) )
import Control.Monad.Trans.State ( StateT, modify, runStateT )

class (MonadBase b m) => MonadScoped b m where
  scoped :: b () -> m ()

type IOResourceT = StateT (IO ())

instance (MonadBase IO m) => MonadScoped IO (IOResourceT m) where
  scoped a = modify (>>a)

runIOResourceT :: (MonadIO m) => IOResourceT m a -> m a
runIOResourceT scope = do
  (a,cleanup) <- runStateT scope (return ())
  liftIO cleanup
  return a
