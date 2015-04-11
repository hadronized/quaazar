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

module Quaazar.Render.Lit where

import Control.Lens
import Data.Monoid ( Monoid(..) )
import Data.Foldable ( for_ )
import Quaazar.Core.Light
import Quaazar.Core.Transform
import Quaazar.Render.Camera ( GPUCamera )
import Quaazar.Render.GL.Buffer ( Buffer )
import Quaazar.Render.GL.Framebuffer ( Framebuffer )
import Quaazar.Render.GL.Shader ( (@=) )
import Quaazar.Render.Light
import Quaazar.Render.Lighting
import Quaazar.Render.Rendered ( Rendered(..) )
import Quaazar.Render.Shaded ( Shaded(..) )

newtype Lit = Lit {
    unLit :: Framebuffer
          -> Buffer
          -> Maybe (ShadowConf,Shadows)
          -> GPUCamera
          -> IO ()
  }

lighten :: Ambient -> [(Omni,Transform)] -> Shaded -> Lit
lighten ambient omnis shd = Lit $ \fb omniBuffer shadowsConf gcam ->
  unShaded shd fb omniBuffer shadowsConf gcam ambient omnis
