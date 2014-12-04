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
import Photon.Core.Light ( Light )
import Photon.Render.PostFX ( PostFX )

data RenderEffect
  = Display
  | Screenshot FilePath
    deriving (Eq,Show)

display :: (Effect RenderEffect m) => m ()
display = react Display

screenshot :: (Effect RenderEffect m) => FilePath -> m ()
screenshot = react . Screenshot
