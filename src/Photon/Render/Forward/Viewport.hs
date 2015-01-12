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

module Photon.Render.Forward.Viewport where

import Control.Lens ( makeLenses )
import Photon.Render.Forward.Post ( Post )

data Viewport = Viewport {
    _viewportWidth  :: Float
  , _viewportHeight :: Float
  , _viewportX      :: Float
  , _viewportY      :: Float
  , _viewportPost   :: Post
  }

makeLenses ''Viewport
