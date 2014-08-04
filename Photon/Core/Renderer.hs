{-# LANGUAGE RankNTypes #-}

module Photon.Core.Renderer where

import Control.Monad.Trans ( MonadIO )
import Photon.Core.Entity ( Entities )
import Photon.Core.Scene ( Scene, IndexPath )

data Renderer frame = Renderer {
    -- |
    render :: forall a. Scene a -> Entities IndexPath -> frame
    -- |
  , display :: (MonadIO m) => frame -> m ()
    -- |
  , screenshot :: (MonadIO m) => FilePath -> frame -> m ()
  }
