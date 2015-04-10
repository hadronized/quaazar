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

module Quaazar.Render.Looked where

import Quaazar.Core.Projection ( Projection )
import Quaazar.Core.Transform ( Transform )
import Quaazar.Render.Camera ( gpuCamera )
import Quaazar.Render.Lighting
import Quaazar.Render.Shaded ( Shaded(..) )
import Quaazar.Render.GL.Buffer ( Buffer )
import Quaazar.Render.GL.Framebuffer ( Framebuffer )
import Quaazar.Render.Light ( ShadowConf )
import Quaazar.Render.Lighting ( Shadows )

newtype Looked = Looked {
    unLooked :: Framebuffer                -- ^ lighting framebuffer
             -> Buffer                     -- ^ omni light buffer
             -> Maybe (ShadowConf,Shadows) -- ^ shadows configuration
             -> IO ()
  }

look :: Projection -> Transform -> Shaded -> Looked
look proj ent shaded = Looked $ \lightingFB omniBuffer shadowsConf ->
  unShaded shaded lightingFB omniBuffer shadowsConf (gpuCamera proj ent)
