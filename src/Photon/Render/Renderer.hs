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
  = Display Float Float Float Projection Entity
    deriving (Eq,Show)

display :: (Effect RenderEffect m) => Float -> Float -> Float -> Projection -> Entity -> m ()
display cr cg cb proj e = react (Display cr cg cb proj e)
