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
import Linear
import Photon.Core.Material ( Albedo ) -- FIXME: Photon.Core.Albedo
import Photon.Render.Forward.Rendered ( Rendered(..) )
import Photon.Render.GL.Shader ( Uniform )
import Photon.Render.Material ( GPUMaterial(..) )

data Shaded = Shaded {
    unShaded :: Uniform (M44 Float)
             -> Uniform Albedo
             -> Uniform Albedo
             -> Uniform Float
             -> IO ()
  , unShadedNoMaterial :: Uniform (M44 Float) -> IO ()
  }

instance Monoid Shaded where
  mempty = Shaded (\_ _ _ _ -> return ()) (\_ -> return ())
  Shaded f g `mappend` Shaded f' g' =
    Shaded (\m d s sh -> f m d s sh >> f' m d s sh) (\m -> g m >> g' m)

shade :: GPUMaterial -> Rendered -> Shaded
shade gmat rdrd = Shaded shade_ shadeNoMaterial
  where
    shade_ modelU matDiffAlbU matSpecAlbU matShnU = do
        runMaterial gmat matDiffAlbU matSpecAlbU matShnU
        unRendered rdrd modelU
    shadeNoMaterial = unRendered rdrd
