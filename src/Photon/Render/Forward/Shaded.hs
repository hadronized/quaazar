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

module Photon.Render.Forward.Shaded where

import Control.Lens
import Data.Monoid ( Monoid(..) )
import Photon.Render.Forward.Lighting
import Photon.Render.Forward.Rendered ( Rendered(..) )
import Photon.Render.Forward.Shadowing
import Photon.Render.Material ( GPUMaterial(..) )

data Shaded = Shaded {
    unShaded :: Lighting -> IO ()
  , unShadedNoMaterial :: Shadowing -> IO ()
  }

instance Monoid Shaded where
  mempty = Shaded (const $ return ()) (const $ return ())
  Shaded f g `mappend` Shaded f' g' = Shaded (\l -> f l >> f' l) (\s -> g s >> g' s)

shade :: GPUMaterial -> Rendered -> Shaded
shade gmat rdrd = Shaded shade_ shadeNoMaterial
  where
    shade_ lighting = do
        runMaterial gmat (lunis^.lightMatDiffAlbU) (lunis^.lightMatSpecAlbU)
          (lunis^.lightMatShnU)
        unRendered rdrd (lunis^.lightModelU)
      where
        lunis = lighting^.lightUniforms
    shadeNoMaterial shadowing =
      unRendered rdrd (shadowing^.shadowUniforms.shadowModelU)
