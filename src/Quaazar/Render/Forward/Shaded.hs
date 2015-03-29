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

module Quaazar.Render.Forward.Shaded where

import Control.Lens
import Quaazar.Render.Camera ( GPUCamera(..) )
import Quaazar.Render.Forward.Accumulation ( Accumulation )
import Quaazar.Render.Forward.Lighting
import Quaazar.Render.Forward.Lit ( Lit(..) )
import Quaazar.Render.GL.Shader ( unused )
import Quaazar.Render.Shader ( GPUProgram(..) )

data Shaded = Shaded {
    unShaded :: Lighting
             -> Accumulation
             -> GPUCamera
             -> IO ()
  }

shade :: GPUProgram mat -> Lit mat -> Shaded
shade gprog lt = Shaded $ \lighting accumulation gcam -> do
  useProgram gprog
  runCamera gcam camProjViewUniform unused eyeUniform
  unLit lt lighting accumulation (sendToProgram gprog)
