module Photon.Core.Light where

import Photon.Core.Color ( Color )

data Light
  = Omni Color
    deriving (Eq,Show)
