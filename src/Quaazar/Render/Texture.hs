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

module Quaazar.Render.Texture (
    -- * GPU texture
    GPUTexture(..) -- FIXME: this should be hidden from the user
  , gpuTexture
    -- FIXME: we need a Core abstraction for that
    -- * Re-exported
  , Filter(..)
  , Wrap(..)
    -- * Resource
  , GPUTextureManager
  ) where

import Control.Monad.Trans ( MonadIO(..) )
import Data.IORef ( modifyIORef, newIORef, readIORef, writeIORef )
import Data.Map as M ( delete, empty, insert, lookup )
import Data.Vector ( toList )
import Numeric.Natural ( Natural )
import Quaazar.Core.Resource ( Manager(..), Resource(..) )
import Quaazar.Core.Texture ( TexelFormat(..), Texture(..), TextureManager )
import Quaazar.Render.GL.GLObject
import Quaazar.Render.GL.Texture ( Filter(..), Wrap(..) )
import qualified Quaazar.Render.GL.Texture as GL

newtype GPUTexture = GPUTexture { bindTextureAt :: Natural -> IO () }

instance Resource (TextureManager,GL.Wrap,GL.Filter) GPUTexture where
  manager _ = do
      ref <- liftIO $ newIORef empty
      return $ Manager (retrieve_ ref) (release_ ref)
    where
      retrieve_ ref (texMgr,wrap,flt) name = do
        mp <- liftIO $ readIORef ref
        case M.lookup name mp of
          Just r -> return r
          Nothing -> do
            tex <- retrieve texMgr () name
            r <- gpuTexture wrap flt tex
            liftIO . writeIORef ref $ insert name r mp
            return r
      release_ ref name = liftIO . modifyIORef ref $ delete name

type GPUTextureManager = Manager (TextureManager,GL.Wrap,GL.Filter) GPUTexture

gpuTexture :: (MonadScoped IO m,MonadIO m)
           => GL.Wrap
           -> GL.Filter
           -> Texture
           -> m GPUTexture
gpuTexture wrap flt (Texture w h format texels) = do
    tex :: GL.Texture2D <- genObject
    GL.bindTextureAt tex 0
    GL.setTextureWrap tex wrap
    GL.setTextureFilters tex flt
    GL.setTextureStorage tex ift w h
    GL.transferPixels tex w h ft (toList texels)
    return . GPUTexture $ GL.bindTextureAt tex
  where
    (ft,ift) = case format of
      R -> (GL.R,GL.R32F)
      RG -> (GL.RG,GL.RG32F)
      RGB -> (GL.RGB,GL.RGB32F)
      RGBA -> (GL.RGBA,GL.RGBA32F)
