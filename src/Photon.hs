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

module Photon (
    -- *
    withPhoton
    -- * Re-exported
  , module Photon.Control
  , module Photon.Core
  , module Photon.Render
  , module Photon.Technics
  , module Photon.Utils
  ) where

import Control.Applicative
import Control.Concurrent.STM ( TVar, atomically, modifyTVar, newTVarIO
                              , readTVar, writeTVar )
import Data.List ( intercalate )
import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW as GLFW
import Numeric.Natural ( Natural )
import Photon.Control
import Photon.Core
import Photon.Event as E
import Photon.Render
import Photon.Technics
import Photon.Utils

withPhoton :: Natural -- ^ Width of the window
           -> Natural -- ^ Height of the window
           -> Bool -- ^ Should the window be fullscreen?
           -> String -- ^ Title of the window
           -> (Window -> IO [Event] -> IO ()) -- ^ Application
           -> IO ()
withPhoton w h full title app = do
    initiated <- GLFW.init
    if initiated then do
      glfwVersion <- fmap showGLFWVersion getVersion
      print (Log InfoLog CoreLog $ "GLFW " ++ glfwVersion ++ " initialized!")
      monitor <- if full then getPrimaryMonitor else return Nothing
      windowHint (WindowHint'ContextVersionMajor 3)
      windowHint (WindowHint'ContextVersionMinor 3)
      createWindow (fromIntegral w) (fromIntegral h) title monitor Nothing >>= \win -> case win of
        Just window -> makeContextCurrent win >> withWindow window app
        -- TODO: display OpenGL information
        Nothing -> print (Log ErrorLog CoreLog "unable to create window :(")
      print (Log InfoLog CoreLog "bye!")
      terminate
      else do
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
  glClearColor 0 0 0 0

-- |Helper function to show 'GLFW.Version' type, because they didn’t pick the
-- one from "Data.Version"…
showGLFWVersion :: Version -> String
showGLFWVersion (Version major minor rev) = intercalate "." $ map show [major,minor,rev]

-------------------------------------------------------------------------------
-- Callbacks
handleKey :: TVar [Event] -> Window -> GLFW.Key -> Int -> GLFW.KeyState -> ModifierKeys -> IO ()
handleKey events _ k _ s _ = atomically . modifyTVar events $ (++ keys)
  where
    keys = case s of
      KeyState'Pressed   -> key KeyPressed
      KeyState'Released  -> key KeyReleased
      KeyState'Repeating -> key KeyReleased
    key st = case k of
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
        Key'Equal        -> r E.Equal
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
        Key'R            -> r E.R
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
        Key'Right        -> r E.Right
        Key'Left         -> r E.Left
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
        r x = [KeyEvent $ st x]

handleMouseButton :: TVar [Event] -> Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> ModifierKeys -> IO ()
handleMouseButton events _ b s _ = atomically . modifyTVar events $ (++ [MouseButtonEvent mouseEvent])
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

handleMouseMotion :: TVar (Double,Double) -> TVar [Event] -> Window -> Double -> Double -> IO ()
handleMouseMotion xy' events _ x y = do
    (x',y') <- atomically $ readTVar xy' <* writeTVar xy' (x,y)
    atomically . modifyTVar events $ (++ [MouseMotionEvent $ MouseMotion x y (x-x') (y-y')])

handleWindowClose :: TVar [Event] -> Window -> IO ()
handleWindowClose events _ = atomically . modifyTVar events $ (++ [WindowEvent Closed,SystemEvent Quit])

handleWindowFocus :: TVar [Event] -> Window -> FocusState -> IO ()
handleWindowFocus events _ f = atomically . modifyTVar events $ (++ [WindowEvent focusEvent])
  where
    focusEvent = case f of
      FocusState'Focused -> FocusGained
      FocusState'Defocused -> FocusLost
