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

module Photon.Interface.Game.Command (
    -- * Game commands
    GameCmd(..)
  , Game
  , registerMesh
  , load
  , registerMaterial
  , renderMeshes
  , registerLight
  , switchLightOn
  , registerCamera
  , look
  , log_
  , time
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
import Photon.Utils.TimePoint ( TimePoint )

data GameCmd n
  = RegisterMesh Mesh (GPUMesh -> n)
  | forall a. (Load a) => LoadObject String (Maybe a -> n)
  | RegisterMaterial Material (GPUMaterial -> n)
  | RenderMeshes GPUMaterial [GPUMesh] n
  | RegisterLight Light (GPULight -> n)
  | SwitchLightOn GPULight Entity n
  | RegisterCamera Projection Entity (GPUCamera -> n)
  | Look GPUCamera n
  | Log LogType String n
  | Time (TimePoint -> n)

instance Functor GameCmd where
  fmap f a = case a of
    RegisterMesh m g -> RegisterMesh m (f . g)
    LoadObject n g -> LoadObject n (f . g)
    RegisterMaterial m g -> RegisterMaterial m (f . g)
    RenderMeshes mat mshs n -> RenderMeshes mat mshs (f n)
    RegisterLight l g -> RegisterLight l (f . g)
    SwitchLightOn l ent n -> SwitchLightOn l ent (f n)
    RegisterCamera proj view g -> RegisterCamera proj view (f . g)
    Look c n -> Look c (f n)
    Log t s n -> Log t s (f n)
    Time g -> Time (f . g)

type Game = Free GameCmd

registerMesh :: Mesh -> Game GPUMesh
registerMesh m = Free (RegisterMesh m Pure)

load :: (Load a) => String -> Game (Maybe a)
load name = Free (LoadObject name Pure)

registerMaterial :: Material -> Game GPUMaterial
registerMaterial m = Free (RegisterMaterial m Pure)

renderMeshes :: GPUMaterial -> [GPUMesh] -> Game ()
renderMeshes mat mshs = Free . RenderMeshes mat mshs $ Pure ()

registerLight :: Light -> Game GPULight
registerLight l = Free (RegisterLight l Pure)

switchLightOn :: GPULight -> Entity -> Game ()
switchLightOn l ent = Free . SwitchLightOn l ent $ Pure ()

registerCamera :: Projection -> Entity -> Game GPUCamera
registerCamera proj view = Free (RegisterCamera proj view Pure)

look :: GPUCamera -> Game ()
look c = Free . Look c $ Pure ()

log_ :: LogType -> String -> Game ()
log_ t s = Free . Log t s $ Pure ()

time :: Game TimePoint
time = Free (Time Pure)
