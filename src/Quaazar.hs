-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Quaazar (
    -- *
    withQuaazar
  , setWindowTitle
    -- * Re-exported
  , Window
  , module X
  ) where

import Control.Concurrent.STM ( TVar, atomically, modifyTVar, newTVarIO
                              , readTVar, writeTVar )
import Data.List ( intercalate )
import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW as GLFW hiding ( setWindowTitle )
import qualified Graphics.UI.GLFW as GLFW ( setWindowTitle )
import Numeric.Natural ( Natural )
import Quaazar.Control as X
import Quaazar.Geometry as X
import Quaazar.Lighting as X
import Quaazar.Render as X
import Quaazar.Scene as X
import Quaazar.System as X
import Quaazar.System.Event as E
import Quaazar.Technics as X
import Quaazar.Utils as X

withQuaazar :: Natural -- ^ Width of the window
            -> Natural -- ^ Height of the window
            -> Bool -- ^ Should the window be fullscreen?
            -> String -- ^ Title of the window
            -> (Window -> IO [Event] -> IO ()) -- ^ Application
            -> IO ()
withQuaazar w h full title app = do
    initiated <- GLFW.init
    if initiated then do
      glfwVersion <- fmap showGLFWVersion getVersion
      print (Log InfoLog CoreLog $ "GLFW " ++ glfwVersion ++ " initialized!")
      monitor <- if full then getPrimaryMonitor else return Nothing
      windowHint (WindowHint'ContextVersionMajor 4)
      windowHint (WindowHint'ContextVersionMinor 4)
      createWindow (fromIntegral w) (fromIntegral h) title monitor Nothing >>= \win -> case win of
        Just window -> makeContextCurrent win >> withWindow window app
        -- TODO: display OpenGL information
        Nothing -> print (Log ErrorLog CoreLog "unable to create window :(")
      print (Log InfoLog CoreLog "bye!")
      terminate
      else
        print (Log ErrorLog CoreLog "unable to init :(")

withWindow :: Window -> (Window -> IO [Event] -> IO ()) -> IO ()
withWindow window app = do
    -- transaction variables
    events <- newTVarIO []
    mouseXY <- newTVarIO (0,0)
    -- callbacks
    setKeyCallback window (Just $ handleKey events)
    setMouseButtonCallback window (Just $ handleMouseButton events)
    setCursorPosCallback window (Just $ handleMouseMotion mouseXY events)
    setWindowCloseCallback window (Just $ handleWindowClose events)
    setWindowFocusCallback window (Just $ handleWindowFocus events)
    -- pre-process
    getCursorPos window >>= atomically . writeTVar mouseXY
    initGL
    -- user app
    app window $ do
      GLFW.pollEvents
      atomically $ readTVar events <* writeTVar events []

initGL :: IO ()
initGL = do
  glEnable gl_DEPTH_TEST
  glEnable gl_TEXTURE_CUBE_MAP_SEAMLESS
  glEnable gl_FRAMEBUFFER_SRGB
 -- glEnable gl_CULL_FACE
--  glCullFace gl_FRONT
  glClearColor 0 0 0 0

setWindowTitle :: Window -> String -> IO ()
setWindowTitle = GLFW.setWindowTitle

-- |Helper function to show 'GLFW.Version' type, because they didn’t pick the
-- one from "Data.Version"…
showGLFWVersion :: Version -> String
showGLFWVersion (Version major minor rev) = intercalate "." $ map show [major,minor,rev]

-------------------------------------------------------------------------------
-- Callbacks
handleKey :: TVar [Event] -> Window -> GLFW.Key -> Int -> GLFW.KeyState -> ModifierKeys -> IO ()
handleKey events _ k _ s _ = atomically . modifyTVar events $ (++ key)
  where
    key = [KeyEvent (fromGLFWKey k) (fromGLFWKeyState s)]

handleMouseButton :: TVar [Event] -> Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> ModifierKeys -> IO ()
handleMouseButton events _ b s _ = atomically . modifyTVar events $ (++ button)
  where
    button = [MouseButtonEvent (fromGLFWMouseButton b) (fromGLFWMouseButtonState s)]

handleMouseMotion :: TVar (Double,Double) -> TVar [Event] -> Window -> Double -> Double -> IO ()
handleMouseMotion xy' events _ x y = do
    (x',y') <- atomically $ readTVar xy' <* writeTVar xy' (x,y)
    atomically . modifyTVar events $ (++ [MouseMotionEvent $ MouseMotion x y (x-x') (y-y')])

handleWindowClose :: TVar [Event] -> Window -> IO ()
handleWindowClose events _ = atomically . modifyTVar events $ (++ [WindowEvent Closed,SystemEvent Quit])

handleWindowFocus :: TVar [Event] -> Window -> FocusState -> IO ()
handleWindowFocus events _ f = atomically . modifyTVar events $ (++ focus)
  where
    focus = [WindowEvent $ fromGLFWFocusState f]
