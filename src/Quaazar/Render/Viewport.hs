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

module Quaazar.Render.Viewport where

import Control.Lens ( makeLenses )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )

data Viewport = Viewport {
    _viewportX      :: Int
  , _viewportY      :: Int
  , _viewportWidth  :: Natural
  , _viewportHeight :: Natural
  }

makeLenses ''Viewport

setViewport :: Viewport -> IO ()
setViewport (Viewport x y w h) = glViewport (fromIntegral x) (fromIntegral y)
  (fromIntegral w) (fromIntegral h)
