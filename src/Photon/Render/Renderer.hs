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
  , screenshot
  ) where

import Control.Applicative
import Photon.Core.Effect
import Photon.Core.Entity ( Entity )
import Photon.Core.Light ( Light )
import Photon.Core.Projection ( Projection )
import Photon.Render.PostFX ( PostFX )

data RenderEffect
  = Display (Entity Projection)
  | Screenshot (Entity Projection) FilePath
    deriving (Eq,Show)

display :: (Effect RenderEffect m) => Entity Projection -> m ()
display = react . Display

screenshot :: (Effect RenderEffect m) => Entity Projection -> FilePath -> m ()
screenshot proj path = react (Screenshot proj path)
