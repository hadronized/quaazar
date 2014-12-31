{-# LANGUAGE ExistentialQuantification #-}

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

module Photon.Interface.Command (
    -- * Photon commands
    PhotonCmd(..)
  , Photon
  , gpu
  , load
  , render
  , log_
  , destroy
  ) where

import Control.Monad.Free
import Photon.Core.Entity ( Entity )
import Photon.Core.Light ( Light )
import Photon.Core.Loader ( Load )
import Photon.Core.Material ( Material )
import Photon.Core.Mesh ( Mesh )
import Photon.Core.PostFX
import Photon.Core.Projection ( Projection )
import Photon.Render.Camera ( GPUCamera )
import Photon.Render.Light ( GPULight )
import Photon.Render.Material ( GPUMaterial )
import Photon.Render.Mesh ( GPUMesh )
import Photon.Render.PostFX ( GPUPostFX )
import Photon.Utils.Log ( LogType )

-- TODO: “Register” is a really bad name there :)
data PhotonCmd n
  = RegisterMesh Mesh (GPUMesh -> n)
  | forall a. (Load a) => LoadObject String (Maybe a -> n)
  | RegisterMaterial Material (GPUMaterial -> n)
  | Render GPUCamera [(GPULight,Entity)] [(GPUMaterial,[(GPUMesh,Entity)])] n
  | RegisterLight Light (GPULight -> n)
  | RegisterCamera Projection Entity (GPUCamera -> n)
  | RegisterPostFX PostFX (Maybe GPUPostFX -> n)
  | Log LogType String n
  | Destroy

instance Functor PhotonCmd where
  fmap f a = case a of
    RegisterMesh m g -> RegisterMesh m (f . g)
    LoadObject n g -> LoadObject n (f . g)
    RegisterMaterial m g -> RegisterMaterial m (f . g)
    Render gcam glig meshes n -> Render gcam glig meshes (f n)
    RegisterLight l g -> RegisterLight l (f . g)
    RegisterCamera proj view g -> RegisterCamera proj view (f . g)
    RegisterPostFX pfx g -> RegisterPostFX pfx (f . g)
    Log t s n -> Log t s (f n)
    Destroy -> Destroy

-- TODO: this should be a newtype
type Photon = Free PhotonCmd

class GPU a b | b -> a where
  gpu :: a -> Photon b

instance GPU Mesh GPUMesh where
  gpu = registerMesh

instance GPU Material GPUMaterial where
  gpu = registerMaterial

instance GPU Light GPULight where
  gpu = registerLight

instance GPU (Projection,Entity) GPUCamera where
  gpu = uncurry registerCamera

instance GPU PostFX (Maybe GPUPostFX) where
  gpu = registerPostFX

registerMesh :: Mesh -> Photon GPUMesh
registerMesh m = Free (RegisterMesh m Pure)

load :: (Load a) => String -> Photon (Maybe a)
load name = Free (LoadObject name Pure)

registerMaterial :: Material -> Photon GPUMaterial
registerMaterial m = Free (RegisterMaterial m Pure)

render :: GPUCamera -> [(GPULight,Entity)] -> [(GPUMaterial,[(GPUMesh,Entity)])] -> Photon ()
render gcam glig meshes = Free (Render gcam glig meshes $ Pure ())

registerLight :: Light -> Photon GPULight
registerLight l = Free (RegisterLight l Pure)

registerCamera :: Projection -> Entity -> Photon GPUCamera
registerCamera proj view = Free (RegisterCamera proj view Pure)

registerPostFX :: PostFX -> Photon (Maybe GPUPostFX)
registerPostFX pfx = Free (RegisterPostFX pfx Pure)

log_ :: LogType -> String -> Photon ()
log_ t s = Free . Log t s $ Pure ()

destroy :: Photon a
destroy = Free Destroy
