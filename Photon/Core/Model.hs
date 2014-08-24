module Photon.Core.Model where

import Control.Lens ( makeLenses )
import Photon.Core.Color ( Color )

data Model = Model {
    _modelColor :: Color
  } deriving (Eq,Show)

makeLenses ''Model
