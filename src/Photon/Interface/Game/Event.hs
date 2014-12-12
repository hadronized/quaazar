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

module Photon.Interface.Game.Event (
    -- * Events
    Event(..)
  , EventHandler
    -- * Key
  , Key(..)
  , KeyState(..)
    -- * Mouse
  , MouseButton(..)
  , MouseMotion(..)
    -- * Window
  , WindowState(..)
    -- * System
  , SystemState(..)
  ) where

data Key
  = Unknown
  | Space
  | Apostrophe
  | Comma
  | Minus
  | Period
  | Slash
  | Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Semicolon
  | Equal
  | A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  | P
  | Q
  | R
  | S
  | T
  | U
  | V
  | W
  | X
  | Y
  | Z
  | LeftBracket
  | Backslash
  | RightBracket
  | GraveAccent
  | World1
  | World2
  | Escape
  | Enter
  | Tab
  | Backspace
  | Insert
  | Delete
  | Right
  | Left
  | Down
  | Up
  | PageUp
  | PageDown
  | Home
  | End
  | CapsLock
  | ScrollLock
  | NumLock
  | PrintScreen
  | Pause
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12
  | F13
  | F14
  | F15
  | F16
  | F17
  | F18
  | F19
  | F20
  | F21
  | F22
  | F23
  | F24
  | F25
  | Pad0
  | Pad1
  | Pad2
  | Pad3
  | Pad4
  | Pad5
  | Pad6
  | Pad7
  | Pad8
  | Pad9
  | PadDecimal
  | PadDivide
  | PadMultiply
  | PadSubtract
  | PadAdd
  | PadEnter
  | PadEqual
  | LeftShift
  | LeftControl
  | LeftAlt
  | LeftSuper
  | RightShift
  | RightControl
  | RightAlt
  | RightSuper
  | Menu
    deriving (Eq,Ord,Read,Show)

data KeyState
  = KeyPressed Key
  | KeyReleased Key
    deriving (Eq,Read,Show)

data MouseButton
  = MouseLeft
  | MouseMiddle
  | MouseRight
  | Mouse4
  | Mouse5
  | Mouse6
  | Mouse7
  | Mouse8
    deriving (Eq,Ord,Read,Show)

data MouseButtonState
  = ButtonPressed MouseButton
  | ButtonReleased MouseButton
    deriving (Eq,Ord,Read,Show)

data MouseMotion = MouseMotion {
    mouseX  :: Double
  , mouseY  :: Double
  , mouseRX :: Double
  , mouseRY :: Double
  } deriving (Eq,Read,Show)

data WindowState
  = Closed
  | Opened
  | FocusLost
  | FocusGained
    deriving (Eq,Read,Show)

data SystemState
  = Quit
    deriving (Eq,Read,Show)

data Event
  = KeyEvent KeyState
  | MouseButtonEvent MouseButtonState
  | MouseMotionEvent MouseMotion
  | WindowEvent WindowState
  | SystemEvent SystemState
    deriving (Eq,Read,Show)

type EventHandler u a = Either u Event -> a -> Maybe a
