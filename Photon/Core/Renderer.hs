{-# LANGUAGE RankNTypes #-}

module Photon.Core.Renderer (
    -- * Renderer
    Renderer(..)
  ) where

import Control.Monad.Trans ( MonadIO )
import Photon.Core.Entity ( Entities )
import Photon.Core.Scene ( IndexPath )

-- |Renderer are plain data that host rendering functions. They’re generated
-- for a specific `Scene a`.
data Renderer frame = Renderer {
    -- |Render scene entities into a frame. This frame is just a rendered
    -- version of the scene, nothing more.
    render :: Entities IndexPath -> frame
    -- |Displaye a rendered scene (frame). That function should render the
    -- frame on the screen / dedicated area for rendering.
  , display :: (MonadIO m) => frame -> m ()
    -- |Write a frame in a file as a PNG image.
  , screenshot :: (MonadIO m) => FilePath -> frame -> m ()
  }
