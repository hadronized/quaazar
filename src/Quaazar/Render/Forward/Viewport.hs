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

module Quaazar.Render.Forward.Viewport where

import Control.Lens ( makeLenses )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )

data Viewport = Viewport {
    _viewportWidth  :: Natural
  , _viewportHeight :: Natural
  , _viewportX      :: Int
  , _viewportY      :: Int
  }

makeLenses ''Viewport

setViewport :: Viewport -> IO ()
setViewport (Viewport w h x y) = glViewport (fromIntegral x) (fromIntegral y)
  (fromIntegral w) (fromIntegral h)
