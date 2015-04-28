{-# LANGUAGE ConstraintKinds #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- This module enhances a little bit "Control.Monad" by adding a few useful
-- functions.
----------------------------------------------------------------------------

module Quaazar.Control.Constraint (
    -- * Quaazar constraint
    MonadQuaazar
  ) where

import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO )
import Quaazar.Utils.Log ( Log, MonadLogger )
import Quaazar.Utils.Scoped ( MonadScoped )

-- |A lot of functions require 'MonadIO', 'MonadScoped', 'MonadLogger' or
-- 'MonadError' constraints. 'MonadQuaazar' gathers them all.
type MonadQuaazar m = (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m)
