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
    -- * Re-exported
    module X
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
