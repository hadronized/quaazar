module Photon.Core.Mesh (
    -- *
    Mesh(..)
    -- * Re-exported modules
  , module Photon.Core.Vertex
  , module Photon.Core.VGroup
  ) where

import Photon.Core.Vertex
import Photon.Core.VGroup

data Mesh = Mesh {
    meshVertices :: Vertices
  , meshVGroup   :: VGroup
  } deriving (Eq,Show)
