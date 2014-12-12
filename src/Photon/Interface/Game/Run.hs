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

import Control.Applicative
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
import qualified Prelude as E ( Left, Right )

data GameDriver = GameDriver {
    drvRegisterMesh     :: Mesh -> IO GPUMesh
  , drvRegisterMaterial :: Material -> IO GPUMaterial
  , drvRegisterLight    :: Light -> IO GPULight
  , drvRegisterCamera   :: (Projection,Entity) -> IO GPUCamera
  , drvLoadObject       :: (Load a) => String -> IO a
  , drvRenderMeshes     :: GPUMaterial -> [(GPUMesh,Entity)] -> IO ()
  , drvSwitchLightOn    :: (GPULight,Entity) -> IO ()
  , drvLook             :: GPUCamera -> IO ()
  , drvLog              :: LogType -> String -> IO ()
  }

showGLFWVersion :: Version -> String
showGLFWVersion (Version major minor rev) = intercalate "." $ map show [major,minor,rev]

-------------------------------------------------------------------------------
-- Run game
runGame :: Natural
        -> Natural
        -> String
        -> IO [e]
        -> EventHandler e a
        -> (a -> Game a)
        -> (Natural -> Natural -> a)
        -> IO ()
runGame w h title pollUserEvents eventHandler step initApp = do
    initiated <- GLFW.init
    if initiated then do
      glfwVersion <- fmap showGLFWVersion getVersion
      print (Log InfoLog CoreLog $ "GLFW " ++ glfwVersion ++ " initialized!")
      windowHint (WindowHint'ContextVersionMajor 3)
      windowHint (WindowHint'ContextVersionMinor 3)
      createWindow (fromIntegral w) (fromIntegral h) title Nothing Nothing >>= \win -> case win of
        Just window -> makeContextCurrent win >> runWithWindow window pollUserEvents eventHandler step app
        -- TODO: display OpenGL information
        Nothing -> print (Log ErrorLog CoreLog "unable to create window :(")
      print (Log InfoLog CoreLog "bye!")
      terminate
      else do
        print (Log ErrorLog CoreLog "unable to init :(")
  where
    app = initApp w h

runWithWindow :: Window -> IO [e] -> EventHandler e a -> (a -> Game a) -> a -> IO ()
runWithWindow window pollUserEvents eventHandler step initializedApp = do
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

    run_ events initializedApp
  where
    run_ events app = do
      userEvs <- fmap (map Left) pollUserEvents
      GLFW.pollEvents
      evs <- fmap (userEvs++) . atomically $ readTVar events <* writeTVar events []
      app' <- routeEvents evs app
      case app' of
        Just appWithEvents -> do
          interpretGame (step appWithEvents)
          run_ events appWithEvents
        Nothing -> return () -- end of application requested
    routeEvents evs app = case evs of
      [] -> Just app
      (e:es) -> case eventHandler e app of
        Just app' -> routeEvents xs app'
        Nothing -> Nothing

-------------------------------------------------------------------------------
-- Game interpreter

-- |This function generates the 'GameDriver'. It uses **OpenGL** to get all the
-- required functions. The width and the height of the window are required in
-- order to be able to generate framebuffers, textures or any kind of object
-- viewport-related.
--
-- If the window’s dimensions change, the game driver should be recreated.
gameDriver :: Natural -> Natural -> Bool -> IO GameDriver
gameDriver width height fullscreen = do
    -- create light program here
    -- map light program’s uniforms here as well
  where
    renderMeshes_ mat meshes = do
      runMaterial mat lightProgram
      liftIO $ traverse_ (uncurry $ renderMesh lightProgram) meshes

-------------------------------------------------------------------------------
-- Callbacks
handleKey :: TVar [Either u Event] -> Window -> GLFW.Key -> Int -> GLFW.KeyState -> ModifierKeys -> IO ()
handleKey events _ k _ s _ = atomically . modifyTVar events $ (++ keys)
  where
    keys = case s of
      KeyState'Pressed   -> key KeyPressed
      KeyState'Released  -> key KeyReleased
      KeyState'Repeating -> key KeyReleased
    key s = case k of
        Key'Unknown      -> []
        Key'Space        -> r Space
        Key'Apostrophe   -> r Apostrophe
        Key'Comma        -> r Comma
        Key'Minus        -> r Minus
        Key'Period       -> r Period
        Key'Slash        -> r Slash
        Key'0            -> r Zero
        Key'1            -> r One
        Key'2            -> r Two
        Key'3            -> r Three
        Key'4            -> r Four
        Key'5            -> r Five
        Key'6            -> r Six
        Key'7            -> r Seven
        Key'8            -> r Eight
        Key'9            -> r Nine
        Key'Semicolon    -> r Semicolon
        Key'Equal        -> r Equal
        Key'A            -> r A
        Key'B            -> r B
        Key'C            -> r C
        Key'D            -> r D
        Key'E            -> r E
        Key'F            -> r F
        Key'G            -> r G
        Key'H            -> r H
        Key'I            -> r I
        Key'J            -> r J
        Key'K            -> r K
        Key'L            -> r L
        Key'M            -> r M
        Key'N            -> r N
        Key'O            -> r O
        Key'P            -> r P
        Key'Q            -> r Q
        Key'R            -> r R
        Key'S            -> r S
        Key'T            -> r T
        Key'U            -> r U
        Key'V            -> r V
        Key'W            -> r W
        Key'X            -> r X
        Key'Y            -> r Y
        Key'Z            -> r Z
        Key'LeftBracket  -> r LeftBracket
        Key'Backslash    -> r Backslash
        Key'RightBracket -> r RightBracket
        Key'GraveAccent  -> r GraveAccent
        Key'World1       -> r World1
        Key'World2       -> r World2
        Key'Escape       -> r Escape
        Key'Enter        -> r Enter
        Key'Tab          -> r Tab
        Key'Backspace    -> r Backspace
        Key'Insert       -> r Insert
        Key'Delete       -> r Delete
        Key'Right        -> r Right
        Key'Left         -> r Left
        Key'Down         -> r Down
        Key'Up           -> r Up
        Key'PageUp       -> r PageUp
        Key'PageDown     -> r PageDown
        Key'Home         -> r Home
        Key'End          -> r End
        Key'CapsLock     -> r CapsLock
        Key'ScrollLock   -> r ScrollLock
        Key'NumLock      -> r NumLock
        Key'PrintScreen  -> r PrintScreen
        Key'Pause        -> r Pause
        Key'F1           -> r F1
        Key'F2           -> r F2   
        Key'F3           -> r F3   
        Key'F4           -> r F4   
        Key'F5           -> r F5   
        Key'F6           -> r F6   
        Key'F7           -> r F7   
        Key'F8           -> r F8   
        Key'F9           -> r F9   
        Key'F10          -> r F10   
        Key'F11          -> r F11   
        Key'F12          -> r F12   
        Key'F13          -> r F13   
        Key'F14          -> r F14   
        Key'F15          -> r F15   
        Key'F16          -> r F16   
        Key'F17          -> r F17   
        Key'F18          -> r F18   
        Key'F19          -> r F19   
        Key'F20          -> r F20   
        Key'F21          -> r F21   
        Key'F22          -> r F22   
        Key'F23          -> r F23   
        Key'F24          -> r F24   
        Key'F25          -> r F25   
        Key'Pad0         -> r Pad0   
        Key'Pad1         -> r Pad1   
        Key'Pad2         -> r Pad2   
        Key'Pad3         -> r Pad3   
        Key'Pad4         -> r Pad4   
        Key'Pad5         -> r Pad5   
        Key'Pad6         -> r Pad6   
        Key'Pad7         -> r Pad7   
        Key'Pad8         -> r Pad8   
        Key'Pad9         -> r Pad9   
        Key'PadDecimal   -> r PadDecimal   
        Key'PadDivide    -> r PadDivide   
        Key'PadMultiply  -> r PadMultiply   
        Key'PadSubtract  -> r PadSubtract   
        Key'PadAdd       -> r PadAdd   
        Key'PadEnter     -> r PadEnter   
        Key'PadEqual     -> r PadEqual   
        Key'LeftShift    -> r LeftShift   
        Key'LeftControl  -> r LeftControl   
        Key'LeftAlt      -> r LeftAlt   
        Key'LeftSuper    -> r LeftSuper   
        Key'RightShift   -> r RightShift   
        Key'RightControl -> r RightControl   
        Key'RightAlt     -> r RightAlt   
        Key'RightSuper   -> r RightSuper   
        Key'Menu         -> r Menu            
      where
        r x = [E.Right . KeyEvent $ s x]

handleMouseButton :: TVar [Either u Event] -> Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> ModifierKeys -> IO ()
handleMouseButton events _ b s _ = atomically . modifyTVar events $ (++ [E.Right $ MouseButtonEvent mouseEvent])
  where
    mouseEvent = case s of
      MouseButtonState'Pressed -> ButtonPressed button
      MouseButtonState'Released -> ButtonReleased button
    button = case b of
      MouseButton'1 -> MouseLeft
      MouseButton'2 -> MouseRight
      MouseButton'3 -> MouseMiddle
      MouseButton'4 -> Mouse4
      MouseButton'5 -> Mouse5
      MouseButton'6 -> Mouse6
      MouseButton'7 -> Mouse7
      MouseButton'8 -> Mouse8

handleMouseMotion :: TVar (Double,Double) -> TVar [Either u Event] -> Window -> Double -> Double -> IO ()
handleMouseMotion xy' events _ x y = do
    (x',y') <- atomically (readTVar xy')
    atomically . modifyTVar events $ (++ [E.Right . MouseMotionEvent $ MouseMotion x y (x-x') (y-y')])

handleWindowClose :: TVar [Either u Event] -> Window -> IO ()
handleWindowClose events _ = atomically . modifyTVar events $ (++ map E.Right [WindowEvent Closed,SystemEvent Quit])

handleWindowFocus :: TVar [Either u Event] -> Window -> FocusState -> IO ()
handleWindowFocus events _ f = atomically . modifyTVar events $ (++ [E.Right $ WindowEvent focusEvent])
  where
    focusEvent = case f of
      FocusState'Focused -> FocusGained
      FocusState'Defocused -> FocusLost
