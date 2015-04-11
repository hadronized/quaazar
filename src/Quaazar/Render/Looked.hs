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
import Quaazar.Render.GL.Buffer ( Buffer )
import Quaazar.Render.GL.Framebuffer ( Framebuffer )
import Quaazar.Render.Light ( ShadowConf )
import Quaazar.Render.Lighting ( Shadows )
import Quaazar.Render.Lit ( Lit(..) )

newtype Looked = Looked {
    unLooked :: Framebuffer                -- ^ lighting framebuffer
             -> Buffer                     -- ^ omni light buffer
             -> Maybe (ShadowConf,Shadows) -- ^ shadows configuration
             -> IO ()
  }

look :: Projection -> Transform -> Lit -> Looked
look proj ent lt = Looked $ \fb omniBuffer shadowsConf ->
  unLit lt fb omniBuffer shadowsConf (gpuCamera proj ent)
