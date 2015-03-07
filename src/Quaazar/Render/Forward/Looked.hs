-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Quaazar.Render.Forward.Looked where

import Quaazar.Core.Projection ( Projection )
import Quaazar.Core.Entity ( Entity )
import Quaazar.Render.Camera ( gpuCamera )
import Quaazar.Render.Forward.Accumulation
import Quaazar.Render.Forward.Lighting
import Quaazar.Render.Forward.Shaded ( Shaded(..) )
import Quaazar.Render.Forward.Shadowing

newtype Looked = Looked { unLooked :: Lighting -> Shadowing -> Accumulation -> IO () }

look :: Projection -> Entity -> Shaded -> Looked
look proj ent shaded = Looked look_
  where
    look_ lighting shadowing accumulation = do
      purgeAccumulationFramebuffer accumulation
      unShaded shaded lighting shadowing accumulation (gpuCamera proj ent)
