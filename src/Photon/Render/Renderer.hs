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
  , renderMesh
  , display
  , screenshot
  ) where

import Control.Applicative
import Photon.Core.Effect
import Photon.Core.Entity ( Entity )
import Photon.Core.Light ( Light )
import Photon.Core.Mesh ( Mesh )
import Photon.Render.PostFX ( PostFX )

data RenderEffect
  = RenderMesh (Managed Mesh) (Managed (Entity Mesh))
  | Display
  | Screenshot FilePath
    deriving (Eq,Show)

renderMesh :: (Effect RenderEffect m)
           => Managed Mesh
           -> Managed (Entity (Mesh))
           -> m ()
renderMesh m e = react (RenderMesh m e)

display :: (Effect RenderEffect m) => m ()
display = react Display

screenshot :: (Effect RenderEffect m) => FilePath -> m ()
screenshot = react . Screenshot
