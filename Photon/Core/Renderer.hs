{-# LANGUAGE RankNTypes #-}

module Photon.Core.Renderer where

import Control.Monad.Trans ( MonadIO )
import Photon.Core.Entity ( Entities )
import Photon.Core.Scene ( IndexPath )

data Renderer frame = Renderer {
    -- |
    render :: Entities IndexPath -> frame
    -- |
  , display :: (MonadIO m) => frame -> m ()
    -- |
  , screenshot :: (MonadIO m) => FilePath -> frame -> m ()
  }
