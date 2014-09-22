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

module Photon.Core.Vertex (
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad ( forM_, void )
import Control.Monad.Trans ( lift )
import Control.Monad.Trans.State ( evalStateT, get, put )
import Data.Aeson
import Data.Aeson.Types ( Parser )
import Data.List ( foldl1' )
import Data.Scientific ( toBoundedInteger )
import Data.Semigroup ( Semigroup(..) )
import Data.Word ( Word8, Word32 )
import Foreign.Marshal.Array ( advancePtr, allocaArray, copyArray )
import Foreign.Ptr ( Ptr, castPtr )
import Foreign.Storable ( sizeOf )
import Numeric.Natural ( Natural )
import qualified Foreign.Marshal.Array as FFI ( withArray )

data Vertex = Vertex {
    -- |Position.
    vertexPosition :: Position
    -- |Normal.
  , vertexNormal   :: Normal
    -- |UV channels.
  , vertexUVChans  :: [UV]
  } deriving (Eq,Ord,Show)

data Vertices
  = Interleaved [Vertex]
  | Deinterleaved [Position] [Normal] [[UV]]
    deriving (Eq,Show)

-- |Group '[Vertex]' into interleaved 'Vertices'.
interleaved :: [Vertex] -> Vertices
interleaved = Interleaved

-- FIXME: the foldr1 might be very slow, so we can use a foldl1' and
-- DList to maximize append speed.
-- |Group '[Vertex]' into deinterleaved 'Vertices'.
deinterleaved :: [Vertex] -> Vertices
deinterleaved = foldr1 rezip
  where
    rezip (Vertex p n uvs) (ap,an,auvs) = (p:ap,n:an,uvs:auvs)

-- |Expose 'Vertices' as a C pointer. This function is the only way to
-- poke such lists through *FFI*.
--
-- The function that treats the C pointer takes a @Ptr Word8@ that represents
-- the 'Vertices' and its size in bytes.
withDeinterleaved :: Vertices -> (Int -> Ptr Word8 -> IO a) -> IO a
withDeinterleaved v f = do
    allocaArray totalBytes $ \buf -> do
      copyVC buf
      f totalBytes buf
  where
    totalBytes = sum (map vertexCompBytes v)
    copyVC b = void . flip evalStateT b $ do
        forM_ v $ \c -> do
          let bytes = vertexCompBytes c
          p <- get
          put $ p `advancePtr` bytes
          let copy x = FFI.withArray x $ \bufx -> copyArray p (castPtr bufx)  bytes
          lift $ foldVertexComp copy copy copy c 
