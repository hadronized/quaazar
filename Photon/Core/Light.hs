module Photon.Core.Light where

import Photon.Core.Color ( Color )

data Light
  = Omni Color
  | Sun Color
  | Spot Float Color
    deriving (Eq,Show)
