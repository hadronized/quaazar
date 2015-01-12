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
import Photon.Render.Material ( GPUMaterial(..) )

newtype Shaded = Shaded { unShaded :: Lighting -> IO () }

instance Monoid Shaded where
  mempty = Shaded . const $ return ()
  Shaded f `mappend` Shaded g = Shaded $ \l -> f l >> g l

shade :: GPUMaterial -> Rendered -> Shaded
shade gmat rdrd = Shaded shade_
  where
    shade_ lighting = do
        runMaterial gmat (lunis^.lightMatDiffAlbU) (lunis^.lightMatSpecAlbU)
          (lunis^.lightMatShnU)
        unRendered rdrd lighting
      where
        lunis = lighting^.lightUniforms
