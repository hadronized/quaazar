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
-- Vertices are points in space with extra information attached. This
-- module exports everything you need to handle vertices and all associated
-- types.
--
-- You’ll find useful functions, such as 'deinterleave' and
-- 'withDeinterleave' you can use to deinterleave vertices in order to
-- optimize/whatever evil you plan to do with ;)
----------------------------------------------------------------------------

module Photon.Core.Vertex where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Photon.Core.Normal ( Normal )
import Photon.Core.Position ( Position )
import Photon.Core.UV ( UV )

data Vertex = Vertex {
    -- |Position.
    _vertexPosition :: Position
    -- |Normal.
  , _vertexNormal   :: Normal
    -- |UV channels.
  , _vertexUVChans  :: [UV]
  } deriving (Eq,Ord,Show)

instance FromJSON Vertex where
  parseJSON = withObject "vertex" $ \o ->
    Vertex
      <$> o .: "position"
      <*> o .: "normal"
      <*> o .: "uvs"

makeLenses ''Vertex

data Vertices
  = Interleaved [Vertex]
  | Deinterleaved [Position] [Normal] [[UV]]
    deriving (Eq,Show)

instance FromJSON Vertices where
  parseJSON = withObject "vertices" $ \o -> do
    int <- o .: "interleaved" .!= True
    if int then
      Interleaved <$> o .: "values"
      else
        Deinterleaved <$> o .: "positions" <*> o .: "normals" <*> o .: "uvs"

-- |Group '[Vertex]' into interleaved 'Vertices'.
interleaved :: [Vertex] -> Vertices
interleaved = Interleaved

-- FIXME: the foldr1 might be very slow, so we can use a foldl1' and
-- DList to maximize append speed.
-- |Group '[Vertex]' into deinterleaved 'Vertices'.
deinterleaved :: [Vertex] -> Vertices
deinterleaved verts = let (p,n,uvs) = foldr rezip ([],[],[]) verts in Deinterleaved p n uvs
  where
    rezip (Vertex p n uvs) (ap,an,auvs) = (p:ap,n:an,uvs:auvs)
