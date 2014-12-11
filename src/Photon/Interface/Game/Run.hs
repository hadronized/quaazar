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

import Control.Concurrent.STM ( atomically )
import Control.Concurrent.STM.TVar ( TVar, modifyTVar, newTVarIO, readTVar
                                   , writeTVar )
import Control.Monad ( forever, unless )
import Data.List ( intercalate )
import Data.Maybe ( fromMaybe )
import Graphics.UI.GLFW as GLFW
import Numeric.Natural ( Natural )
import Photon.Core.Entity ( Entity )
import Photon.Core.Light ( Light )
import Photon.Core.Loader ( Load )
import Photon.Core.Material ( Material )
import Photon.Core.Mesh ( Mesh )
import Photon.Core.Projection ( Projection )
import Photon.Interface.Game.Command ( GameCmd, Game )
import Photon.Interface.Game.Event
import Photon.Render.Camera ( GPUCamera )
import Photon.Render.Light ( GPULight )
import Photon.Render.Material ( GPUMaterial )
import Photon.Render.Mesh ( GPUMesh )
import Photon.Utils.Log ( Log(..), LogCommitter(..), LogType(..) )

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

showGLFWVersion :: Version -> String
showGLFWVersion (Version maj min rev) = intercalate "." $ map show [maj,min,rev]

runGame :: Natural
        -> Natural
        -> String
        -> IO [Event]
        -> EventHandler a
        -> GameImpl
        -> (a -> Game a)
        -> a
        -> IO ()
runGame w h title pollEvents handler gameImpl step app = do
  initiated <- GLFW.init
  if initiated then do
    glfwVersion <- fmap showGLFWVersion getVersion
    print (Log InfoLog CoreLog $ "GLFW " ++ glfwVersion ++ " initialized!")
    windowHint (WindowHint'ContextVersionMajor 3)
    windowHint (WindowHint'ContextVersionMinor 3)
    createWindow (fromIntegral w) (fromIntegral h) title Nothing Nothing >>= \w -> case w of
      Just window -> makeContextCurrent w >> runWithWindow window
      Nothing -> print (Log ErrorLog CoreLog "unable to create window :(")
    print (Log InfoLog CoreLog "bye!")
    terminate
    else do
      print (Log ErrorLog CoreLog "unable to init :(")
{-
  where
    run app = pollEvents >>= forwardEvents app >>= maybe (return ()) (run . logic)
    forwardEvents app events = return $ case events of
      [] -> Just app
      (x:xs) -> handler (x :| xs) app
      -}

runWithWindow :: Window -> IO ()
runWithWindow window = do
    events <- newTVarIO []

    run_
  where
    run_ = do
      pollEvents
      --events <- getEvents
      closed <- windowShouldClose window
      unless closed run_

handleWindowClose :: TVar [Event] -> Window -> IO ()
handleWindowClose events _ = atomically . modifyTVar events $ (++ [WindowEvent Closed,SystemEvent Quit])

{-
getEvents :: Window -> IO [Event]
getEvents =
-}
