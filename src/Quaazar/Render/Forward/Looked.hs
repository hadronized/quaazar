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

module Quaazar.Render.Forward.Looked where

import Control.Lens
import Quaazar.Render.Camera ( GPUCamera )
import Quaazar.Render.Forward.Accumulation
import Quaazar.Render.Forward.Lighting
import Quaazar.Render.Forward.Shaded ( Shaded(..) )
import Quaazar.Render.Forward.Shadowing
import Quaazar.Render.Forward.Viewport ( Viewport )

newtype Looked = Looked { unLooked :: Viewport -> Lighting -> Shadowing -> Accumulation -> IO () }

look :: GPUCamera -> Shaded -> Looked
look gpucam shd = Looked look_
  where
    look_ screenViewport lighting shadowing accumulation = do
        purgeAccumulationFramebuffer accumulation
        pushCameraToLighting lighting gpucam
        unShaded shd (lunis^.lightModelU) (lunis^.lightMatDiffAlbU)
          (lunis^.lightMatSpecAlbU) (lunis^.lightMatShnU)
      where
        lunis = lighting^.lightUniforms
