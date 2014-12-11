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

import Data.List.NonEmpty ( NonEmpty(..) )

newtype Key = Key { unKey :: Char } deriving (Eq,Ord,Read,Show)

data KeyState
  = KeyPressed Key
  | KeyReleased Key
    deriving (Eq,Read,Show)

data MouseButton
  = MouseLeft
  | MouseMiddle
  | MouseRight
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
  | MouseButtonEvent [MouseButton]
  | MouseMotionEvent MouseMotion
  | WindowEvent WindowState
  | SystemEvent SystemState
    deriving (Eq,Read,Show)

type EventHandler a = NonEmpty Event -> a -> Maybe a
