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

module Photon.Render.Renderer (
    -- * Reaction
    RenderEffect(..)
  , display
  ) where

import Control.Applicative
import Photon.Core.Effect
import Photon.Core.Entity ( Entity )
import Photon.Core.Light ( Light )
import Photon.Core.Projection ( Projection )
import Photon.Render.PostFX ( PostFX )

data RenderEffect
  = Display Projection (Entity Projection)
    deriving (Eq,Show)

display :: (Effect RenderEffect m) => Projection -> Entity Projection -> m ()
display proj e = react (Display proj e)
