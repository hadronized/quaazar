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
standalone w h full title app = do
  initiated <- GLFW.init
  if
    | initiated -> do
        glfwVersion <- fmap showGLFWVersion getVersion
        print (Log InfoLog CoreLog $ "GLFW " ++ glfwVersion ++ " initialized!")
        monitor <- if full then getPrimaryMonitor else return Nothing
        windowHint (WindowHint'ContextVersionMajor 4)
        windowHint (WindowHint'ContextVersionMinor 4)
        createWindow (fromIntegral w) (fromIntegral h) title monitor Nothing >>= \win -> case win of
          Just window -> makeContextCurrent win >> withWindow window app
          Nothing -> print (Log ErrorLog CoreLog "unable to create window")
        print (Log InfoLog CoreLog "bye!")
        terminate
    | otherwise -> print $ Log ErrorLog CoreLog "unable to init"

withWindow :: Window
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
withWindow window app = do
    -- futures
    (key,pushKey) <- newFuture
    (mouse,clickMouse) <- newFuture
    (cursor,moveCursor) <- newFuture
    (windowClosed,closeWindow) <- newFuture
    (focus,changeFocus) <- newFuture
    -- callbacks
    setKeyCallback window . Just $ \_ k _ s _ ->
      pushKey (fromGLFWKey k,fromGLFWKeyState s)
    setMouseButtonCallback window . Just $ \_ b s _ ->
      clickMouse (fromGLFWMouseButton b,fromGLFWMouseButtonState s)
    setCursorPosCallback window . Just $ \_ x y -> moveCursor (x,y)
    setWindowCloseCallback window . Just $ \_ -> closeWindow ()
    setWindowFocusCallback window . Just $ \_ f ->
      changeFocus (fromGLFWFocusState f)
    -- pre-process
    initGL
    -- run the whole shit
    app window key mouse cursor windowClosed focus pollEvents

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
