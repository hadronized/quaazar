-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Quaazar.System.Run.Live where

import Control.Concurrent
import Control.Concurrent.Future
import Data.IORef
import Numeric.Natural ( Natural )
import Quaazar.System.Event
import Quaazar.System.Run.Startup

data Session = Session {
    -- |
    kill :: IO ()
    -- |
  , pause :: IO ()
  }

startupLive :: Natural
            -> Natural
            -> Bool
            -> String
            -> IO Session
startupLive w h full title = do
  -- shared references
  quitRef <- newIORef False
  pauseRef <- newIORef False
  -- fork
  tid <- forkIO $ startup w h full title (liveSession quitRef pauseRef)
  pure $ Session (writeIORef quitRef True) (modifyIORef pauseRef not)

liveSession :: IORef Bool
            -> IORef Bool
            -> Window
            -> Future (Key,KeyState)
            -> Future (MouseButton,MouseButtonState)
            -> Future (Double,Double)
            -> Future ()
            -> Future Bool
            -> IO ()
            -> IO ()
liveSession killRef pauseRef win key mouse cursor closed focused pollEvents = go
  where
    go = do
      killed <- readIORef killRef
      paused <- readIORef pauseRef
      if
        | killed -> pure ()
        | otherwise -> do
          -- threadDelay?
          if
            | paused -> go
            | otherwise -> do
              -- do an actual thing
              go
