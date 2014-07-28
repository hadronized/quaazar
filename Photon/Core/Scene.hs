{-# LANGUAGE OverloadedStrings #-} -- for tests only

module Photon.Core.Scene where

import Photon.Core.Color ( Color )
import Photon.Core.Light ( Light )
-- import Photon.Core.Mesh ( Mesh )
import Photon.Core.Projection ( Projection )

import Photon.Core.Projection ( Projection(..) )
import Photon.Core.Light ( Light(..) )
import Photon.Core.Mesh
import Linear

newtype Scene a = Scene [Top a] deriving (Eq,Functor,Show)

data Top a
  = Cam a Projection
  | Msh a Mesh [Mdl a]
  | Lig a Light
    deriving (Eq,Functor,Show)

data Mdl a = Mdl a Color deriving (Eq,Functor,Show)

scene :: Scene String
scene = Scene
    [
      Cam "cam0" $ Perspective 0 0 0 0
    , Cam "cam1" $ Perspective 0 1 0 pi
    , Lig "redLight" $ Sun red
    , Msh "cube" cube [Mdl "redCube" red]
    ]
  where
    cube   = Mesh verts vgroup
    verts  = Vertices [] []
    vgroup = Triangles []
    red    = V4 1 0 0 1
