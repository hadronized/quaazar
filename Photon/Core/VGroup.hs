module Photon.Core.VGroup (
    -- * Grouping
    Line(..)
  , Triangle(..)
  , fromLine
  , fromTriangle
    -- * Vertex group
  , VGroup(..)
  , fromVGroup
  ) where

import Data.Word ( Word32 )

data Line = Line Word32 Word32 deriving (Eq,Read,Show)
data Triangle = Triangle Word32 Word32 Word32 deriving (Eq,Read,Show)

data VGroup
  = Points [Word32]
  | Lines [Line]
  | Triangles [Triangle]
  | SLines Word32 [Word32]
  | STriangles Word32 Word32 [Word32]
    deriving (Eq,Read,Show)

fromLine :: Line -> [Word32]
fromLine (Line a b) = [a,b]

fromTriangle :: Triangle -> [Word32]
fromTriangle (Triangle a b c) = [a,b,c]

fromVGroup :: VGroup -> [Word32]
fromVGroup vg = case vg of
    Points p         -> p
    Lines l          -> concat (fmap fromLine l)
    Triangles t      -> concat (fmap fromTriangle t)
    SLines a l       -> a : l
    STriangles a b l -> a : b : l
