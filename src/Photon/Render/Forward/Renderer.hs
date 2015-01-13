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
import Control.Monad.Trans.Either ( runEitherT )
import Graphics.UI.GLFW ( Window )
import Numeric.Natural ( Natural )
import Photon.Render.Forward.Accumulation ( Accumulation, getAccumulation )
import Photon.Render.Forward.Lighting ( Lighting, getLighting )
import Photon.Render.Forward.Shadowing ( Shadowing, getShadowing )
import Photon.Utils.Log ( Log )

data ForwardRenderer = ForwardRenderer {
    _frLighting :: Lighting
  , _frShadowing :: Shadowing
  , _frAccumulation :: Accumulation
  , _frWindow :: Window
  }

getForwardRenderer :: Natural -> Natural -> Window -> IO (Either Log ForwardRenderer)
getForwardRenderer w h window = runEitherT $
  ForwardRenderer
    <$> getLighting w h
    <*> getShadowing 1024 0.1 100
    <*> getAccumulation w h
    <*> pure window
