module Photon.Core.Mesh (
    -- *
    Mesh(Mesh)
  , meshVertices
  , meshVGroup
    -- * Re-exported modules
  , module Photon.Core.Vertex
  , module Photon.Core.VGroup
  ) where

import Control.Lens ( makeLenses )
import Photon.Core.Vertex
import Photon.Core.VGroup

data Mesh = Mesh {
    _meshVertices :: Vertices
  , _meshVGroup   :: VGroup
  } deriving (Eq,Show)

makeLenses ''Mesh
