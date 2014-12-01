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
import Control.Monad.Trans ( MonadIO(..) )
import Control.Monad.Trans.State ( StateT, runStateT )
import Data.Vector as V ( Vector, length )
import Photon.Core.Effect
import Photon.Core.Light
import Photon.Core.Material

newtype OpenGLT m a = OpenGLT { runOpenGLT :: StateT OpenGLSt m a } deriving (Applicative,Functor,Monad)

data OpenGLSt = OpenGLSt {
    _glStLights    :: Vector Light
  , _glStMaterials :: Vector Material
  } deriving (Eq,Show)

makeLenses ''OpenGLSt

-------------------------------------------------------------------------------
-- Light support
instance (Functor m,Monad m) => Effect LightSpawned (OpenGLT m) where
  react (LightSpawned (Managed (H h) l)) = OpenGLT $ do
    sz <- uses glStLights V.length
    if h < sz then
      glStLights . ix h .= l
      else
        glStLights %= flip snoc l

instance (Functor m,Monad m) => Effect LightLost (OpenGLT m) where
  react = const $ return ()

instance (Functor m,Monad m) => Effect LightEffect (OpenGLT m) where
  react e = OpenGLT $ case e of
    ColorChanged lig color -> glStLights . ix (unManage lig) . ligColor .= color
    PowerChanged lig power -> glStLights . ix (unManage lig) . ligPower .= power
    RadiusChanged lig radius -> glStLights . ix (unManage lig) . ligRadius .= radius
    CastShadowsChanged lig sh -> glStLights . ix (unManage lig) . ligCastShadows .= sh

-------------------------------------------------------------------------------
-- Material support
instance (Functor m,Monad m) => Effect MaterialSpawned (OpenGLT m) where
  react (MaterialSpawned (Managed (H h) m)) = OpenGLT $ do
    sz <- uses glStMaterials V.length
    if h < sz then
      glStMaterials . ix h .= m
      else
        glStMaterials %= flip snoc m

instance (Functor m,Monad m) => Effect MaterialLost (OpenGLT m) where
  react = const $ return ()

instance (Functor m,Monad m) => Effect MaterialEffect (OpenGLT m) where
  react e = OpenGLT $ case e of
    DiffuseChanged m diff -> glStMaterials . ix (unManage m) . matDiffuseAlbedo .= diff
    SpecularChanged m spec -> glStMaterials . ix (unManage m) . matSpecularAlbedo .= spec
    ShininessChanged m shn -> glStMaterials . ix (unManage m) . matShininess .= shn

-------------------------------------------------------------------------------
-- Miscellaneous
unManage :: Managed a -> Int
unManage m = m^.handle.to unH
  where unH (H i) = i
