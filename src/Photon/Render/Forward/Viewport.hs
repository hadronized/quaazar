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
import Numeric.Natural ( Natural )
import Photon.Render.Forward.Post ( Post )

data Viewport = Viewport {
    _viewportWidth  :: Natural
  , _viewportHeight :: Natural
  , _viewportX      :: Int
  , _viewportY      :: Int
  , _viewportPost   :: Post
  }

makeLenses ''Viewport
