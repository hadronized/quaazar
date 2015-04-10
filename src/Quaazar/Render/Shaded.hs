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

module Quaazar.Render.Shaded where

import Control.Lens
import Quaazar.Render.Camera ( GPUCamera(..) )
import Quaazar.Render.Lighting
import Quaazar.Render.Lit ( Lit(..) )
import Quaazar.Render.GL.Buffer ( Buffer )
import Quaazar.Render.GL.Framebuffer ( Framebuffer )
import Quaazar.Render.GL.Shader ( unused )
import Quaazar.Render.Light ( ShadowConf )
import Quaazar.Render.Lighting ( Shadows )
import Quaazar.Render.Shader ( GPUProgram(..) )

data Shaded = Shaded {
    unShaded :: Framebuffer                -- ^ lighting framebuffer
             -> Buffer                     -- ^ omni light buffer
             -> Maybe (ShadowConf,Shadows) -- ^ shadows configuration
             -> GPUCamera
             -> IO ()
  }

shade :: GPUProgram mat -> Lit mat -> Shaded
shade gprog lt = Shaded $ \lightingFB omniBuffer shadowsConf gcam -> do
  useProgram gprog
  runCamera gcam camProjViewUniform unused eyeUniform
  unLit lt lightingFB omniBuffer shadowsConf (sendToProgram gprog)
