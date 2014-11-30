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
  , withLight
  , postfx
  , postProcess
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
  | UseLight (Managed Light)
  | UnuseLight (Managed Light)
  | Postprocess (Managed PostFX)
  | Display
  | Screenshot FilePath
    deriving (Eq,Show)

renderMesh :: (Effect RenderEffect m)
           => Managed Mesh
           -> Managed (Entity (Mesh))
           -> m ()
renderMesh m e = react (RenderMesh m e)

withLight :: (Effect RenderEffect m) => Managed Light -> m a -> m a
withLight l a = react (UseLight l) *> a <* react (UnuseLight l)

postfx :: (Effect RenderEffect m) => Managed PostFX -> m ()
postfx = react . Postprocess

postProcess :: (Effect RenderEffect m) => [Managed PostFX] -> m ()
postProcess = sequence_ . map postfx

display :: (Effect RenderEffect m) => m ()
display = react Display

screenshot :: (Effect RenderEffect m) => FilePath -> m ()
screenshot = react . Screenshot
