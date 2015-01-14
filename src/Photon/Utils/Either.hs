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

module Photon.Utils.Either (
    -- * Combinators
    generalizeEither
  ) where

import Control.Monad.Error.Class ( MonadError(throwError) )

-- FIXME: change that when my pull request on either has merged
generalizeEither :: (MonadError e m) => Either e a -> m a
generalizeEither = either throwError return
