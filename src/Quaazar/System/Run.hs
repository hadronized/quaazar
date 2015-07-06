-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Quaazar.System.Run (
    -- * Creating programs
    standalone
  , setWindowTitle
  ) where

import Control.Concurrent.Future
import Data.List ( intercalate )
import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW hiding ( Key, KeyState, MouseButton, MouseButtonState
                               , init, setWindowTitle )
import qualified Graphics.UI.GLFW as GLFW ( init, setWindowTitle )
import Numeric.Natural ( Natural )
import Quaazar.System.Event
import Quaazar.Utils.Log

-- |'standalone w h full title app' spawns a new stand-alone window with its own
-- reaction system (based on "future"’s 'Future').
--
-- 'app' represents the application as a function. It’s passed several 'Future'
-- values:
--
-- @
--      Window
--   -> Future (Key,KeyState)
--   -> Future (MouseButton,MouseButtonState)
--   -> Future (Double,Double)                -- (mouseX,mouseY)
--   -> Future ()                             -- produce when closing the window
--   -> Future Bool                           -- focused
--   -> IO ()                                 -- events poller
--   -> IO ()
-- @
--
-- The @eventsPoller@ is used to poll events and make them available within the
-- 'Future' values.
standalone :: Natural -- ^ Width of the window
           -> Natural -- ^ Height of the window
           -> Bool -- ^ Should the window be fullscreen?
           -> String -- ^ Title of the window
           -> (
                 Window
              -> Future (Key,KeyState)
              -> Future (MouseButton,MouseButtonState)
              -> Future (Double,Double)
              -> Future ()
              -> Future Bool
              -> IO ()
              -> IO ()
              )
           -> IO ()
standalone w h full title app =
  initiate w h full title >>= \case
    Just win -> do
      initGL
      (key,mouse,cursor,windowClosed,focus) <- getFutures win
      app win key mouse cursor windowClosed focus pollEvents
    Nothing -> terminate

initiate :: Natural -> Natural -> Bool -> String -> IO (Maybe Window)
initiate w h full title = do
  initiated <- GLFW.init
  if
    | initiated -> do
        glfwVersion <- fmap showGLFWVersion getVersion
        print $ Log InfoLog CoreLog $ "GLFW " ++ glfwVersion ++ " initialized!"
        monitor <- if full then getPrimaryMonitor else return Nothing
        windowHint $ WindowHint'ContextVersionMajor 4
        windowHint $ WindowHint'ContextVersionMinor 4
        createWindow (fromIntegral w) (fromIntegral h) title monitor Nothing >>= \win -> case win of
          Just _ -> makeContextCurrent win >> pure win
          Nothing -> do
            print (Log ErrorLog CoreLog "unable to create window")
            pure Nothing
    | otherwise -> do
      print $ Log ErrorLog CoreLog "unable to init"
      pure Nothing

getFutures :: Window
           -> IO (
                Future (Key,KeyState)
              , Future (MouseButton,MouseButtonState)
              , Future (Double,Double)
              , Future ()
              , Future Bool
              )
getFutures win = do
  -- futures
  (key,pushKey) <- newFuture
  (mouse,clickMouse) <- newFuture
  (cursor,moveCursor) <- newFuture
  (windowClosed,closeWindow) <- newFuture
  (focus,changeFocus) <- newFuture
  -- callbacks
  setKeyCallback win . Just $ \_ k _ s _ ->
    pushKey (fromGLFWKey k,fromGLFWKeyState s)
  setMouseButtonCallback win . Just $ \_ b s _ ->
    clickMouse (fromGLFWMouseButton b,fromGLFWMouseButtonState s)
  setCursorPosCallback win . Just $ \_ x y -> moveCursor (x,y)
  setWindowCloseCallback win . Just $ \_ -> closeWindow ()
  setWindowFocusCallback win . Just $ \_ f ->
    changeFocus (fromGLFWFocusState f)
  pure (key,mouse,cursor,windowClosed,focus)

-- |Interpreter version of 'standalone'.
--
-- That function can be used in a **ghci** session.
{-
standaloneInterpreter :: Natural -- ^ Width of the window
                      -> Natural -- ^ Height of the window
                      -> Bool -- ^ Should the window be fullscreen?
                      -> String -- ^ Title of the window
                      -> IO (
                            Window
                          , Future (Key,KeyState)
                          , Future (MouseButton,MouseButtonState)
                          , Future (Double,Double)
                          , Future ()
                          , Future Bool
                          , IO ()
                          )
-}



initGL :: IO ()
initGL = do
  glEnable gl_DEPTH_TEST
  glEnable gl_TEXTURE_CUBE_MAP_SEAMLESS
  glEnable gl_FRAMEBUFFER_SRGB
  glClearColor 0 0 0 0

setWindowTitle :: Window -> String -> IO ()
setWindowTitle = GLFW.setWindowTitle

-- |Helper function to show 'GLFW.Version' type, because they didn’t pick the
-- one from "Data.Version"…
showGLFWVersion :: Version -> String
showGLFWVersion (Version major minor rev) = intercalate "." $ map show [major,minor,rev]
