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

module Quaazar.Render.Rendered where

import Data.Monoid ( Monoid(..) )
import Linear ( M44 )
import Quaazar.Core.Transform ( Transform )
import Quaazar.Render.GL.Shader ( Uniform )
import Quaazar.Render.Mesh ( GPUMesh(..) )

newtype Rendered mat = Rendered {
    unRendered :: Uniform (M44 Float) -- ^ model matrix
               -> (mat -> IO ()) -- ^ material properties sink
               -> IO ()
  }

instance Monoid (Rendered mat) where
  mempty = Rendered $ \_ _ -> return ()
  Rendered f `mappend` Rendered g =
    Rendered $ \m s -> f m s >> g m s

render :: GPUMesh -> Transform -> mat -> Rendered mat
render gmsh trsf mat = Rendered $ \modelU sinkMat -> do
  sinkMat mat
  renderMesh gmsh modelU trsf
