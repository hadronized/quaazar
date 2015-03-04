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

module Quaazar.Render.Forward.Shaded where

import Control.Lens
import Data.Monoid ( Monoid(..) )
import Linear
import Quaazar.Render.Camera ( GPUCamera(..) )
import Quaazar.Render.Forward.Accumulation ( Accumulation )
import Quaazar.Render.Forward.Lighting
import Quaazar.Render.Forward.Lit ( Lit(..) )
import Quaazar.Render.Forward.Rendered ( Rendered(..) )
import Quaazar.Render.Forward.Shadowing ( Shadowing )
import Quaazar.Render.GL.Shader ( Uniform, unused )
import Quaazar.Render.Shader ( GPUProgram(..) )

data Shaded = Shaded {
    unShaded :: Lighting
             -> Shadowing
             -> Accumulation
             -> GPUCamera
             -> IO ()
  }

instance Monoid Shaded where
  mempty = Shaded $ \_ _ _ _ -> return ()
  Shaded f `mappend` Shaded g = Shaded $ \l s a c -> f l s a c >> g l s a c

shade :: GPUProgram a -> a -> Lit -> Shaded
shade gprog inputs lt = Shaded $ \lighting shadowing accumulation gcam -> do
  let unis = lighting^.lightUniforms
  useProgram gprog inputs
  runCamera gcam (unis^.lightCamProjViewU) unused (unis^.lightEyeU)
  unLit lt lighting shadowing accumulation
