{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Vertices are points in space with extra information attached:
--
--   - position
--   - normal (can be flat or smooth)
--   - UV coordinates channels
--
-- Currently, only one UV channel is supported.
----------------------------------------------------------------------------

module Quaazar.Geometry.Vertex where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Aeson.Types ( typeMismatch )
import Numeric.Natural ( Natural )
import Quaazar.Geometry.Normal ( Normal )
import Quaazar.Geometry.Position ( Position )
import Quaazar.Geometry.UV ( UV )

-- |A 'Vertex' is a point in space. The system used is a local space
-- to the object the vertex belongs to.
data Vertex = Vertex {
    -- |Position.
    _vertexPosition :: Position
    -- |Normal.
  , _vertexNormal   :: Normal
    -- |UV channels.
  , _vertexUVChans  :: [UV]
  } deriving (Eq,Ord,Show)

instance FromJSON Vertex where
  parseJSON v = do
    a <- parseJSON v
    case a of
      [p,n,u] -> Vertex <$> parseJSON p <*> parseJSON n <*> parseJSON u
      _       -> typeMismatch "vertex" v

makeLenses ''Vertex

-- |A list of vertices is wrapped in a dedicated type called 'Vertices'.
-- In comes in two flavours:
--
--   - 'Interleaved', which just glues 'Vertex' to each other in a list;
--     that’s the definition of interleaved vertices
--   - 'Deinterleaved', wich extracts all components out of vertices and
--     pack them in specific lists
data Vertices
  = Interleaved [Vertex]
  | Deinterleaved Natural [Position] [Normal] [[UV]]
    deriving (Eq,Show)

instance FromJSON Vertices where
  parseJSON = withObject "vertices" $ \o -> do
    int <- o .: "interleaved" .!= True
    if int then
      Interleaved <$> o .: "values"
      else do
        positions <- o .: "positions"
        normals   <- o .: "normals"
        uvs       <- o .: "uvs"
        return $ Deinterleaved (fromIntegral $ length positions) positions normals uvs

-- |Group '[Vertex]' into interleaved 'Vertices'.
interleaved :: [Vertex] -> Vertices
interleaved = Interleaved

-- FIXME: the foldr1 might be very slow, so we can use a foldl1' and
-- 'DList' to maximize append speed.
-- |Group '[Vertex]' into deinterleaved 'Vertices'.
deinterleaved :: [Vertex] -> Vertices
deinterleaved verts =
    let
      (p,n,uvs) = foldr rezip ([],[],[]) verts
      vnb = fromIntegral (length p)
    in
      Deinterleaved vnb p n uvs
  where
    rezip (Vertex p n uvs) (ap,an,auvs) = (p:ap,n:an,uvs:auvs)
