{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Vertices can be grouped by indices through a 'VGroup'.
----------------------------------------------------------------------------

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

import Control.Applicative
import Control.Monad ( mzero )
import Data.Aeson
import Data.Aeson.Types ( modifyFailure )
import Data.Scientific ( toBoundedInteger )
import Data.Word ( Word32 )
import Data.Vector ( toList )

-- |A line is two vertex indices.
data Line = Line Word32 Word32 deriving (Eq,Read,Show)

instance FromJSON Line where
  parseJSON v = withArray "line" parseArray v
    where
      parseArray ar = case toList ar of
        [Number x,Number y] -> case sequence (map toBoundedInteger [x,y]) of
          Just [a,b] -> return (Line a b)
          _ -> mzero
        _ -> mzero

-- |A triangle is thre vertex indices.
data Triangle = Triangle Word32 Word32 Word32 deriving (Eq,Read,Show)

instance FromJSON Triangle where
  parseJSON v = withArray "triangle" parseArray v
    where
      parseArray ar = case toList ar of
        [Number x,Number y,Number z] -> case sequence (map toBoundedInteger [x,y,z]) of
          Just [a,b,c] -> return (Triangle a b c)
          _ -> mzero
        _ -> mzero

-- |Grouping vertices via indices is performed via 'VGroup'. You have
-- several ways of grouping:
--
--   - 'Points' is used to express *no grouping*; the vertices appears as
--     a vertices cloud then;
--   - 'Lines' is used to connect vertices two-by-two as lines;
--   - 'Triangles' is used to connect vertices three-by-three as triangles;
--   - 'SLines' is used to connect vertices as lines;
--   - 'STriangles' is used to connect vertices as triangles.
data VGroup
  = Points [Word32]
  | Lines [Line]
  | Triangles [Triangle]
  | SLines Line [Word32]
  | STriangles Triangle [Word32]
    deriving (Eq,Read,Show)

instance FromJSON VGroup where
  parseJSON v = withObject "vertex group" (\o -> o .: "grouping" >>= withText "grouping" (dispatchGrouping o)) v
    where
      dispatchGrouping o g
        | g == "points"     =  fmap Points (o .: "points")
        | g == "lines"      =  fmap Lines (o .: "lines")
        | g == "triangles"  =  fmap Triangles (o .: "triangles")
        | g == "slines"     =  do
            sline <- o .: "slines"
            SLines <$> sline .: "head" <*> sline .: "tail"
        | g == "striangles" = do
            striangles <- o .: "striangles"
            STriangles <$> striangles .: "head" <*> striangles .: "tail"
        | otherwise         = fail "unknown grouping"

-- |Turn a 'Line' into a list of two indices.
fromLine :: Line -> [Word32]
fromLine (Line a b) = [a,b]

-- |Turn a 'Triangle' into a list of three indices.
fromTriangle :: Triangle -> [Word32]
fromTriangle (Triangle a b c) = [a,b,c]

-- |Turn a 'VGroup' into a list of indices.
fromVGroup :: VGroup -> [Word32]
fromVGroup vg = case vg of
  Points ps      -> ps
  Lines ls       -> concatMap fromLine ls
  Triangles ts   -> concatMap fromTriangle ts
  SLines l i     -> fromLine l ++ i
  STriangles a i -> fromTriangle a ++ i
