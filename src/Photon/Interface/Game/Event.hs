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
  , CoreEvent(..)
  , EventHandler
    -- * Key
  , Key(..)
  , KeyState(..)
    -- * Mouse
  , MouseButton(..)
  , MouseButtonState(..)
  , MouseMotion(..)
    -- * Window
  , WindowState(..)
    -- * System
  , SystemState(..)
  ) where

-- |Keyboard key. The keys are not documented because they’re self-explanatory.
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

-- |If a key has an event attached to it, it might be 'KeyPressed' or
-- 'KeyReleased'. If your application has /repeating/ enabled, several
-- 'KeyPressed' 'KeyReleased' event pairs are generated after the first to
-- emulate the repeating.
data KeyState
  = KeyPressed Key -- ^ Occurs when a key is pressed 
  | KeyReleased Key -- ^ Occurs when a key is released
    deriving (Eq,Read,Show)

-- |Mouse button.
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

-- |If a mouse button has an event attached to it, it might be 'ButtonPressed'
-- or 'ButtonReleased'.
data MouseButtonState
  = ButtonPressed MouseButton
  | ButtonReleased MouseButton
    deriving (Eq,Ord,Read,Show)

-- |When the mouse moves, it generates a 'MouseMotion' event. That event
-- gathers the new position of the mouse (use 'mouseX' and 'mouseY' to get
-- them) along with its relative movement (use 'mouseRX' and 'mouseRY' to get
-- them).
data MouseMotion = MouseMotion {
    mouseX  :: Double -- ^ Mouse position on X
  , mouseY  :: Double -- ^ Mouse position on Y
  , mouseRX :: Double -- ^ Mouse relative movement on X
  , mouseRY :: Double -- ^ Mouse relative movement on Y
  } deriving (Eq,Read,Show)

-- |A window can emit several events: 'Closed' when the user closes it,
-- 'Opened' when the application starts, 'FocusLost' when the window loses the
-- focus and 'FocusGained' and it gets the focus back.
data WindowState
  = Closed
  | Opened
  | FocusLost
  | FocusGained
    deriving (Eq,Read,Show)

-- |System state are other kind of events.
data SystemState
  = Quit -- ^ The application is quitting
    deriving (Eq,Read,Show)

-- |Gather all core events.
data CoreEvent
  = KeyEvent KeyState
  | MouseButtonEvent MouseButtonState
  | MouseMotionEvent MouseMotion
  | WindowEvent WindowState
  | SystemEvent SystemState
    deriving (Eq,Read,Show)

data Event u = CoreEvent CoreEvent | UserEvent u deriving (Eq,Read,Show)

-- |An 'EventHandler u a' handles core event 'Event' and user event 'u' in
-- an application 'a'.
type EventHandler u a = Event u -> a -> Maybe a
