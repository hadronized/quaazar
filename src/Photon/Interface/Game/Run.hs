{-# LANGUAGE RankNTypes #-}

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

module Photon.Interface.Game.Run where

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

data GameImpl = GameImpl {
    implRegisterMesh     :: Mesh -> IO GPUMesh
  , implRegisterMaterial :: Material -> IO GPUMaterial
  , implRegisterLight    :: Light -> IO GPULight
  , implRegisterCamera   :: (Projection,Entity) -> IO GPUCamera
  , implLoadObject       :: (Load a) => String -> IO a
  , implRenderMeshes     :: GPUMaterial -> [GPUMesh] -> IO ()
  , implSwitchLightOn    :: GPULight -> IO ()
  , implLook             :: GPUCamera -> IO ()
  , implLog              :: LogType -> String -> IO ()
  }

{-
runGame :: IO [Event] -> EventHandler a -> (a -> a) -> a -> IO ()
runGame pollEvents handler logic = run
  where
    run app = pollEvents >>= forwardEvents app >>= maybe (return ()) (run . logic)
    forwardEvents app events = return $ case events of
      [] -> Just app
      (x:xs) -> handler (x :| xs) app
-}
