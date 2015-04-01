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
import Quaazar.Render.Forward.Lighting
import Quaazar.Render.Forward.Shaded ( Shaded(..) )
import Quaazar.Render.GL.Buffer ( Buffer )
import Quaazar.Render.GL.Framebuffer ( Framebuffer )

newtype Looked = Looked {
    unLooked :: Framebuffer -- ^ lighting framebuffer
             -> Buffer      -- ^ omni light buffer
             -> IO ()
  }

look :: Projection -> Transform -> Shaded -> Looked
look proj ent shaded = Looked $ \lightingFB omniBuffer ->
  unShaded shaded lightingFB omniBuffer (gpuCamera proj ent)
