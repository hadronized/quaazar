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
import Quaazar.Core.Transform ( Transform )
import Quaazar.Render.Camera ( gpuCamera )
import Quaazar.Render.Forward.Accumulation
import Quaazar.Render.Forward.Lighting
import Quaazar.Render.Forward.Shaded ( Shaded(..) )

newtype Looked = Looked { unLooked :: Lighting -> Accumulation -> IO () }

look :: Projection -> Transform -> Shaded -> Looked
look proj ent shaded = Looked $ \lighting accumulation ->
  unShaded shaded lighting accumulation (gpuCamera proj ent)
