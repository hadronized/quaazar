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

module Quaazar.Render.Texture where

import Control.Monad.Trans ( MonadIO(..) )
import Data.Vector ( toList )
import Numeric.Natural ( Natural )
import Quaazar.Core.Texture ( TexelFormat(..), Texture(..) )
import Quaazar.Render.GL.GLObject
import qualified Quaazar.Render.GL.Texture as GL

newtype GPUTexture = GPUTexture { bindTextureAt :: Natural -> IO () }

gpuTexture :: (MonadScoped IO m,MonadIO m)
           => GL.Wrap
           -> GL.Filter
           -> Texture
           -> m GPUTexture
gpuTexture wrap flt (Texture width height format texels) = do
    tex :: GL.Texture2D <- genObject
    GL.bindTextureAt tex 0
    GL.setTextureWrap tex wrap
    GL.setTextureFilters tex flt
    GL.setTextureImage tex ift width height ft (toList texels)
    return . GPUTexture $ GL.bindTextureAt tex
  where
    (ft,ift) = case format of
      R -> (GL.R,GL.R32F)
      RG -> (GL.RG,GL.RG32F)
      RGB -> (GL.RGB,GL.RGB32F)
      RGBA -> (GL.RGBA,GL.RGBA32F)
