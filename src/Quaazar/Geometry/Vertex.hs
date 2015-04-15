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
-- Vertices are points in space with extra information attached. This
-- module exports everything you need to handle vertices and all associated
-- types.
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
-- DList to maximize append speed.
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
