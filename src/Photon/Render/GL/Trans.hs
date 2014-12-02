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

module Photon.Render.GL.Trans (
    -- *
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad ( unless )
import Control.Monad.Trans ( MonadIO(..) )
import Control.Monad.Trans.State ( StateT, evalStateT )
import Data.Vector as V ( Vector, length )
import Photon.Core.Effect
import Photon.Core.Light
import Photon.Core.Material
import Photon.Core.Mesh
import Photon.Render.GL.Mesh
import Photon.Utils.FreeList
import Prelude hiding ( drop )

newtype OpenGLT m a = OpenGLT (StateT OpenGLSt m a) deriving (Applicative,Functor,Monad)

data OpenGLSt = OpenGLSt {
    _glStDispatch  :: (FreeList,Vector Int)      -- ^ objects dispatcher
  , _glStLights    :: (FreeList,Vector Light)    -- ^ lights
  , _glStMaterials :: (FreeList,Vector Material) -- ^ materials
  , _glStMeshes    :: (FreeList,Vector GPUMesh)  -- ^ meshes
  , _glStMatMeshes :: (FreeList,Vector [H Mesh]) -- ^ meshes handles per material
  } deriving (Eq,Show)

makeLenses ''OpenGLSt

evalOpenGLT :: (Monad m) => OpenGLT m a -> m a
evalOpenGLT (OpenGLT st) = evalStateT st (OpenGLSt empty2 empty2 empty2 empty2 empty2)

dispatchHandle :: (Monad m) => Managed a -> StateT OpenGLSt m Int
dispatchHandle (Managed (H h) _) = use $ singular $ glStDispatch . _2 . ix h

-------------------------------------------------------------------------------
-- Manager
instance (Functor m,Monad m) => Manager (OpenGLT m) where
  manage a = OpenGLT $ do
    (h,fl) <- uses (glStDispatch._1) nextFree
    glStDispatch . _1 .= fl
    sz <- uses (glStDispatch . _2) V.length
    unless (h < sz) (glStDispatch . _2 %= flip snoc 0)
    return (Managed (H h) a)
  drop (Managed (H h) _) = OpenGLT (glStDispatch . _1 %= recycleFree h)

-------------------------------------------------------------------------------
-- Light support
instance (Functor m,Monad m) => Effect LightSpawned (OpenGLT m) where
  react (LightSpawned (Managed (H dHandle) l)) = OpenGLT $ do
    -- light handle
    (lightHandle,lightFL) <- uses (glStLights._1) nextFree
    glStLights._1 .= lightFL
    -- double indirection
    glStDispatch . _2 . ix dHandle .= lightHandle
    -- storing
    sz <- uses (glStLights._2) V.length
    if lightHandle < sz then
      glStLights . _2 . ix lightHandle .= l
      else
        glStLights . _2 %= flip snoc l

instance (Functor m,Monad m) => Effect LightLost (OpenGLT m) where
  react (LightLost lig) = OpenGLT $ do
    lightHandle <- dispatchHandle lig
    glStLights . _1 %= recycleFree lightHandle

instance (Functor m,Monad m) => Effect LightEffect (OpenGLT m) where
  react e = OpenGLT $ case e of
    ColorChanged lig color -> do
      lh <- dispatchHandle lig
      glStLights . _2 . ix lh . ligColor .= color
    PowerChanged lig power -> do
      lh <- dispatchHandle lig
      glStLights . _2 . ix lh . ligPower .= power
    RadiusChanged lig radius -> do
      lh <- dispatchHandle lig
      glStLights . _2 . ix lh . ligRadius .= radius
    CastShadowsChanged lig sh -> do
      lh <- dispatchHandle lig
      glStLights . _2 . ix lh . ligCastShadows .= sh

-------------------------------------------------------------------------------
-- Material support
instance (Functor m,Monad m) => Effect MaterialSpawned (OpenGLT m) where
  react (MaterialSpawned (Managed (H dHandle) m)) = OpenGLT $ do
    -- material handle
    (matHandle,matFL) <- uses (glStMaterials._1) nextFree
    glStMaterials._1 .= matFL
    -- double indirection
    glStDispatch . _2 . ix dHandle .= matHandle
    -- storing
    sz <- uses (glStMaterials._2) V.length
    if matHandle < sz then
      glStMaterials . _2 . ix matHandle .= m
      else
        glStMaterials . _2 %= flip snoc m

instance (Functor m,Monad m) => Effect MaterialLost (OpenGLT m) where
  react (MaterialLost m) = OpenGLT $ do
    matHandle <- dispatchHandle m
    glStMaterials . _1 %= recycleFree matHandle

instance (Functor m,Monad m) => Effect MaterialEffect (OpenGLT m) where
  react e = OpenGLT $ case e of
    DiffuseChanged m diff -> do
      mh <- dispatchHandle m
      glStMaterials . _2 . ix mh . matDiffuseAlbedo .= diff
    SpecularChanged m spec -> do
      mh <- dispatchHandle m
      glStMaterials . _2 . ix mh . matSpecularAlbedo .= spec
    ShininessChanged m shn -> do
      mh <- dispatchHandle m
      glStMaterials . _2 . ix mh . matShininess .= shn

{-
-------------------------------------------------------------------------------
-- Mesh support
instance (Functor m,MonadIO m) => Effect MeshSpawned (OpenGLT m) where
  react (MeshSpawned (Managed (H h) m)) = OpenGLT $ do
    sz <- uses glStMeshes V.length
    gpuData <- liftIO (gpuMesh m)
    if h < sz then
      glStMeshes . ix h .= gpuData
      else
        glStMeshes %= flip snoc gpuData

instance (Functor m,Monad m) => Effect MeshLost (OpenGLT m) where
  react = const $ return ()

instance (Functor m,MonadIO m) => Effect MeshEffect (OpenGLT m) where
  react e = OpenGLT $ case e of
    UseMaterial msh mat -> glStMatMeshes . ix (unManage mat) %= (:) (msh^.handle)

-}
-------------------------------------------------------------------------------
-- Miscellaneous

unManage :: Managed a -> Int
unManage m = m^.handle.to unH
  where unH (H i) = i

empty2 :: (FreeList,Vector a)
empty2 = (freeList,empty)
