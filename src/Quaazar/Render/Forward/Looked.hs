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

import Quaazar.Render.Camera ( GPUCamera )
import Quaazar.Render.Forward.Accumulation
import Quaazar.Render.Forward.Lighting
import Quaazar.Render.Forward.Lit ( Lit(..) )
import Quaazar.Render.Forward.Shadowing
import Quaazar.Render.Forward.Viewport ( Viewport )

newtype Looked = Looked { unLooked :: Viewport -> Lighting -> Shadowing -> Accumulation -> IO () }

look :: GPUCamera -> Lit -> Looked
look gpucam lit = Looked look_
  where
    look_ screenViewport lighting shadowing accumulation = do
      purgeAccumulationFramebuffer accumulation
      pushCameraToLighting lighting gpucam
      unLit lit screenViewport lighting shadowing accumulation gpucam
