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

module Quaazar.Render.Camera where

import Control.Lens
import Data.Maybe ( fromJust )
import Linear ( M44, V3, (!*!), inv44 )
import Quaazar.Render.Transform ( cameraMatrix )
import Quaazar.Render.GL.Shader ( Uniform, (@=) )
import Quaazar.Render.Projection ( Projection, projectionMatrix )
import Quaazar.Scene.Hierarchy ( Instance(..) )
import Quaazar.Scene.Transform ( transformPosition )

data GPUCamera = GPUCamera {
    -- |@runCamera pv pv1 eye@ send the camera to the GPU using uniforms.
    --
    -- 'pv' is a uniform for the /projection * view/.
    --
    -- 'pv1' is a uniform for the inverse of /projection * view/.
    --
    -- 'eye' is a uniform for the /position of the camera/.
    runCamera :: Uniform (M44 Float) -- ^ projection * view
              -> Uniform (M44 Float) -- ^ (projection * view)-1
              -> Uniform (V3 Float) -- ^ eye
              -> IO ()
  , cameraProjection :: M44 Float
  }

gpuCamera :: Instance Projection -> GPUCamera
gpuCamera inst = GPUCamera sendCamera proj'
  where
    sendCamera projViewU iProjViewU eyeU = do
        projViewU @= projView
        iProjViewU @= fromJust (inv44 projView)
        eyeU @= trsf^.transformPosition
    projView = proj' !*! cameraMatrix trsf
    proj' = projectionMatrix $ instCarried inst
    trsf = instTransform inst
