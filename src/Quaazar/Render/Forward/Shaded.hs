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
import Quaazar.Core.Material ( Albedo ) -- FIXME: Quaazar.Core.Albedo
import Quaazar.Render.Forward.Rendered ( Rendered(..) )
import Quaazar.Render.GL.Shader ( Uniform )
import Quaazar.Render.Material ( GPUMaterial(..) )

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
