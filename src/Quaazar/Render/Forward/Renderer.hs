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

module Quaazar.Render.Forward.Renderer where

import Control.Applicative
import Control.Lens ( makeLenses )
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO )
import Graphics.UI.GLFW ( Window )
import Numeric.Natural ( Natural )
import Quaazar.Render.Forward.Accumulation ( Accumulation, getAccumulation )
import Quaazar.Render.Forward.Lighting ( Lighting, getLighting )
import Quaazar.Render.Forward.Shadowing ( Shadowing, getShadowing )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

data ForwardRenderer = ForwardRenderer {
    _frLighting :: Lighting
  , _frShadowing :: Shadowing
  , _frAccumulation :: Accumulation
  , _frWindow :: Window
  }

makeLenses ''ForwardRenderer

getForwardRenderer :: (Applicative m,MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
                   => Natural
                   -> Natural
                   -> Natural
                   -> Natural
                   -> Window
                   -> m ForwardRenderer
getForwardRenderer w h shadowDef lightNb window =
  ForwardRenderer
    <$> getLighting w h lightNb
    <*> getShadowing w h (256+shadowDef*256)
    <*> getAccumulation w h
    <*> pure window
