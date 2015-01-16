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

module Photon.Render.Forward.Renderer where

import Control.Applicative
import Control.Lens ( makeLenses )
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO )
import Graphics.UI.GLFW ( Window )
import Numeric.Natural ( Natural )
import Photon.Render.Forward.Accumulation ( Accumulation, getAccumulation )
import Photon.Render.Forward.Lighting ( Lighting, getLighting )
import Photon.Render.Forward.Shadowing ( Shadowing, getShadowing )
import Photon.Utils.Log

data ForwardRenderer = ForwardRenderer {
    _frLighting :: Lighting
  , _frShadowing :: Shadowing
  , _frAccumulation :: Accumulation
  , _frWindow :: Window
  }

makeLenses ''ForwardRenderer

getForwardRenderer :: (Applicative m,MonadIO m,MonadLogger m,MonadError Log m)
                   => Natural
                   -> Natural
                   -> Window
                   -> m ForwardRenderer
getForwardRenderer w h window =
  ForwardRenderer
    <$> getLighting w h
    <*> getShadowing w h 1024
    <*> getAccumulation w h
    <*> pure window
