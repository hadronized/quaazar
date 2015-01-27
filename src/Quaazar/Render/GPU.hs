-----------------------------------------------------------------------------
-- |
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Quaazar.Render.GPU (
    -- * GPU pushing
    GPU(..)
  ) where

import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO )
import Quaazar.Utils.Log ( Log )

-- |GPU-representable class.
class GPU a g | g -> a where
  -- |'gpu x' represents 'x' on the /GPU/.
  gpu :: (MonadIO m,MonadError Log m) => a -> m g
  {-# MINIMAL gpu #-}
