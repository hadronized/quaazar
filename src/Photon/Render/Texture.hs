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

module Photon.Render.Texture where

import Control.Monad.Trans ( MonadIO(..) )
import Numeric.Natural ( Natural )
import Photon.Core.Texture ( TexelFormat(..), Texture(..) )
import qualified Photon.Render.GL.Texture as GL ( Format(..), InternalFormat(..)
                                           , bindTextureAt )
import Photon.Render.GL.Texture ( Filter(..), Wrap(..), genTexture2D
                                , setTextureFilters, setTextureImage
                                , setTextureWrap )

newtype GPUTexture = GPUTexture { bindTextureAt :: Natural -> IO () }

gpuTexture :: (MonadIO m) => Texture -> Wrap -> Filter -> m GPUTexture
gpuTexture (Texture width height format texels) wrap flt = liftIO $ do
    tex <- genTexture2D
    GL.bindTextureAt tex 0
    setTextureWrap tex wrap
    setTextureFilters tex flt
    setTextureImage tex ift width height ft texels
    return . GPUTexture $ GL.bindTextureAt tex
  where
    (ft,ift) = case format of
      R -> (GL.R,GL.R32F)
      RG -> (GL.RG,GL.RG32F)
      RGB -> (GL.RGB,GL.RGB32F)
      RGBA -> (GL.RGBA,GL.RGBA32F)
