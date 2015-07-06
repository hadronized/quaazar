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

module Quaazar.System.Event (
    -- * Key
    Key(..)
  , KeyState(..)
    -- * Mouse
  , MouseButton(..)
  , MouseButtonState(..)
    -- * Window
  , WindowState(..)
    -- * System
  , SystemState(..)
    -- * Turning GLFW events into quaazar ones
  , fromGLFWKey
  , fromGLFWKeyState
  , fromGLFWMouseButton
  , fromGLFWMouseButtonState
  , fromGLFWFocusState
  ) where

import Graphics.UI.GLFW hiding ( Key, KeyState, MouseButton, MouseButtonState )
import qualified Graphics.UI.GLFW as GLFW ( Key, KeyState, MouseButton
                                          , MouseButtonState )
import Prelude hiding ( Left, Right )

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
-- 'KeyReleased'.
data KeyState
  = KeyPressed -- ^ Occurs when a key is pressed
  | KeyReleased -- ^ Occurs when a key is released
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
  = ButtonPressed
  | ButtonReleased
    deriving (Eq,Ord,Read,Show)

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

-- |Convert a GLFW key into quaazar one.
fromGLFWKey :: GLFW.Key -> Key
fromGLFWKey k = case k of
  Key'Unknown      -> Unknown
  Key'Space        -> Space
  Key'Apostrophe   -> Apostrophe
  Key'Comma        -> Comma
  Key'Minus        -> Minus
  Key'Period       -> Period
  Key'Slash        -> Slash
  Key'0            -> Zero
  Key'1            -> One
  Key'2            -> Two
  Key'3            -> Three
  Key'4            -> Four
  Key'5            -> Five
  Key'6            -> Six
  Key'7            -> Seven
  Key'8            -> Eight
  Key'9            -> Nine
  Key'Semicolon    -> Semicolon
  Key'Equal        -> Equal
  Key'A            -> A
  Key'B            -> B
  Key'C            -> C
  Key'D            -> D
  Key'E            -> E
  Key'F            -> F
  Key'G            -> G
  Key'H            -> H
  Key'I            -> I
  Key'J            -> J
  Key'K            -> K
  Key'L            -> L
  Key'M            -> M
  Key'N            -> N
  Key'O            -> O
  Key'P            -> P
  Key'Q            -> Q
  Key'R            -> R
  Key'S            -> S
  Key'T            -> T
  Key'U            -> U
  Key'V            -> V
  Key'W            -> W
  Key'X            -> X
  Key'Y            -> Y
  Key'Z            -> Z
  Key'LeftBracket  -> LeftBracket
  Key'Backslash    -> Backslash
  Key'RightBracket -> RightBracket
  Key'GraveAccent  -> GraveAccent
  Key'World1       -> World1
  Key'World2       -> World2
  Key'Escape       -> Escape
  Key'Enter        -> Enter
  Key'Tab          -> Tab
  Key'Backspace    -> Backspace
  Key'Insert       -> Insert
  Key'Delete       -> Delete
  Key'Right        -> Right
  Key'Left         -> Left
  Key'Down         -> Down
  Key'Up           -> Up
  Key'PageUp       -> PageUp
  Key'PageDown     -> PageDown
  Key'Home         -> Home
  Key'End          -> End
  Key'CapsLock     -> CapsLock
  Key'ScrollLock   -> ScrollLock
  Key'NumLock      -> NumLock
  Key'PrintScreen  -> PrintScreen
  Key'Pause        -> Pause
  Key'F1           -> F1
  Key'F2           -> F2
  Key'F3           -> F3
  Key'F4           -> F4
  Key'F5           -> F5
  Key'F6           -> F6
  Key'F7           -> F7
  Key'F8           -> F8
  Key'F9           -> F9
  Key'F10          -> F10
  Key'F11          -> F11
  Key'F12          -> F12
  Key'F13          -> F13
  Key'F14          -> F14
  Key'F15          -> F15
  Key'F16          -> F16
  Key'F17          -> F17
  Key'F18          -> F18
  Key'F19          -> F19
  Key'F20          -> F20
  Key'F21          -> F21
  Key'F22          -> F22
  Key'F23          -> F23
  Key'F24          -> F24
  Key'F25          -> F25
  Key'Pad0         -> Pad0
  Key'Pad1         -> Pad1
  Key'Pad2         -> Pad2
  Key'Pad3         -> Pad3
  Key'Pad4         -> Pad4
  Key'Pad5         -> Pad5
  Key'Pad6         -> Pad6
  Key'Pad7         -> Pad7
  Key'Pad8         -> Pad8
  Key'Pad9         -> Pad9
  Key'PadDecimal   -> PadDecimal
  Key'PadDivide    -> PadDivide
  Key'PadMultiply  -> PadMultiply
  Key'PadSubtract  -> PadSubtract
  Key'PadAdd       -> PadAdd
  Key'PadEnter     -> PadEnter
  Key'PadEqual     -> PadEqual
  Key'LeftShift    -> LeftShift
  Key'LeftControl  -> LeftControl
  Key'LeftAlt      -> LeftAlt
  Key'LeftSuper    -> LeftSuper
  Key'RightShift   -> RightShift
  Key'RightControl -> RightControl
  Key'RightAlt     -> RightAlt
  Key'RightSuper   -> RightSuper
  Key'Menu         -> Menu

-- |Convert a GLFW key state into quaazar one.
fromGLFWKeyState :: GLFW.KeyState -> KeyState
fromGLFWKeyState ks = case ks of
  KeyState'Pressed -> KeyPressed
  KeyState'Released -> KeyReleased
  KeyState'Repeating -> KeyPressed

-- |Convert a GLFW mouse button into quaazar one.
fromGLFWMouseButton :: GLFW.MouseButton -> MouseButton
fromGLFWMouseButton b = case b of
  MouseButton'1 -> MouseLeft
  MouseButton'2 -> MouseMiddle
  MouseButton'3 -> MouseRight
  MouseButton'4 -> Mouse4
  MouseButton'5 -> Mouse5
  MouseButton'6 -> Mouse6
  MouseButton'7 -> Mouse7
  MouseButton'8 -> Mouse8

-- |Convert a GLFW mouse button state into quaazar one.
fromGLFWMouseButtonState :: GLFW.MouseButtonState -> MouseButtonState
fromGLFWMouseButtonState bs = case bs of
  MouseButtonState'Pressed -> ButtonPressed
  MouseButtonState'Released -> ButtonReleased

-- |Convert a GLFW window focus into a quaazar one.
fromGLFWFocusState :: FocusState -> Bool
fromGLFWFocusState fs = case fs of
  FocusState'Focused -> True
  FocusState'Defocused -> False
