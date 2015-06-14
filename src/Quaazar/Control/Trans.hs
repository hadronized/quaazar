{-# LANGUAGE StandaloneDeriving #-}

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

module Quaazar.Control.Trans (
    -- * Quaazar monad transformer
    QuaazarT
  , runQuaazarT
  ) where

import Control.Monad.Base ( MonadBase )
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Journal ( MonadJournal )
import Control.Monad.Trans ( MonadIO )
import Control.Monad.Trans.Either ( EitherT, runEitherT )
import Control.Monad.Trans.Journal ( JournalT, runJournalT )
import Quaazar.Utils.Log ( Log, LogQueue )
import Quaazar.Utils.Scoped ( MonadScoped, IOScopedT, runIOScopedT )

-- |The 'QuaazarT' monad transformer.
newtype QuaazarT m a = QuaazarT (
    EitherT Log (IOScopedT (JournalT LogQueue m)) a
  ) deriving (Applicative,Functor,Monad,MonadError Log,MonadIO,MonadJournal LogQueue)

deriving instance (MonadBase IO m) => MonadBase IO (QuaazarT m)
deriving instance (MonadBase IO m) => MonadScoped IO (QuaazarT m)

-- |Run a 'QuaazarT' stack.
runQuaazarT :: (Monad m,MonadIO m) => QuaazarT m a -> m (Either Log a,LogQueue)
runQuaazarT (QuaazarT stack) =
  runJournalT . runIOScopedT . runEitherT $ stack
