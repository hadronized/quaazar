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
import Quaazar.Render.Forward.Shaded ( Shaded(..) )
import Quaazar.Render.Forward.Shadowing

newtype Looked = Looked { unLooked :: Lighting -> Shadowing -> Accumulation -> IO () }

look :: GPUCamera -> Shaded -> Looked
look gpucam shaded = Looked look_
  where
    look_ lighting shadowing accumulation = do
      purgeAccumulationFramebuffer accumulation
      unShaded shaded lighting shadowing accumulation gpucam
