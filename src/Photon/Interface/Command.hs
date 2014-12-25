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
  , look
  , log_
  , destroy
  ) where

import Control.Monad.Free
import Photon.Core.Entity ( Entity )
import Photon.Core.Light ( Light )
import Photon.Core.Loader ( Load )
import Photon.Core.Material ( Material )
import Photon.Core.Mesh ( Mesh )
import Photon.Core.Projection ( Projection )
import Photon.Render.Camera ( GPUCamera )
import Photon.Render.Light ( GPULight )
import Photon.Render.Material ( GPUMaterial )
import Photon.Render.Mesh ( GPUMesh )
import Photon.Utils.Log ( LogType )

data PhotonCmd n
  = RegisterMesh Mesh (GPUMesh -> n)
  | forall a. (Load a) => LoadObject String (Maybe a -> n)
  | RegisterMaterial Material (GPUMaterial -> n)
  | Render GPULight Entity [(GPUMaterial,[(GPUMesh,Entity)])] n
  | RegisterLight Light (GPULight -> n)
  | RegisterCamera Projection Entity (GPUCamera -> n)
  | Look GPUCamera n
  | Log LogType String n
  | Destroy

instance Functor PhotonCmd where
  fmap f a = case a of
    RegisterMesh m g -> RegisterMesh m (f . g)
    LoadObject n g -> LoadObject n (f . g)
    RegisterMaterial m g -> RegisterMaterial m (f . g)
    Render glig ent meshes n -> Render glig ent meshes (f n)
    RegisterLight l g -> RegisterLight l (f . g)
    RegisterCamera proj view g -> RegisterCamera proj view (f . g)
    Look c n -> Look c (f n)
    Log t s n -> Log t s (f n)
    Destroy -> Destroy

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

registerMesh :: Mesh -> Photon GPUMesh
registerMesh m = Free (RegisterMesh m Pure)

load :: (Load a) => String -> Photon (Maybe a)
load name = Free (LoadObject name Pure)

registerMaterial :: Material -> Photon GPUMaterial
registerMaterial m = Free (RegisterMaterial m Pure)

render :: GPULight -> Entity -> [(GPUMaterial,[(GPUMesh,Entity)])] -> Photon ()
render glig ent meshes = Free (Render glig ent meshes $ Pure ())

registerLight :: Light -> Photon GPULight
registerLight l = Free (RegisterLight l Pure)

registerCamera :: Projection -> Entity -> Photon GPUCamera
registerCamera proj view = Free (RegisterCamera proj view Pure)

look :: GPUCamera -> Photon ()
look c = Free . Look c $ Pure ()

log_ :: LogType -> String -> Photon ()
log_ t s = Free . Log t s $ Pure ()

destroy :: Photon a
destroy = Free Destroy