module Photon.Core.Vertex (
    -- * Vertex protocol
    VertexProto
  , VertexCompProto(VertexCompProto)
  , vcProtoType
  , vcProtoNormalized
  , vcProtoSemantic
  , VertexCompType(..)
  , VertexCompSemantic(..)
  , fromVertexCompSemantic
    -- * Vertex
  , VertexComp
  , mapVertexComp
  , vertexCompSize
  , vertexCompBytes
  , integral
  , unsigned
  , floating
  , Vertex
  , Vertices(Vertices)
  , verticesVerts
  , verticesVProto
  --, merge
  , deinterleave
  , withDeinterleaved
  ) where

import Control.Lens
import Control.Monad ( forM_, void )
import Control.Monad.Trans ( lift )
import Control.Monad.Trans.State ( evalStateT, get, put )
import Data.List ( foldl1' )
import Data.Semigroup
import Data.Word ( Word8, Word32 )
import Foreign.Marshal.Array ( advancePtr, allocaArray, copyArray, withArray )
import Foreign.Ptr ( Ptr, castPtr )
import Foreign.Storable ( sizeOf )
import Numeric.Natural ( Natural )

data VertexCompProto = VertexCompProto {
    -- |
    _vcProtoNormalized :: Bool
    -- |
  , _vcProtoSemantic   :: VertexCompSemantic
    -- |
  , _vcProtoType       :: VertexCompType
  } deriving (Eq,Show)

data VertexCompType
  = VInt
  | VInt2
  | VInt3
  | VInt4
  | VUInt
  | VUInt2
  | VUInt3
  | VUInt4
  | VFloat
  | VFloat2
  | VFloat3
  | VFloat4
    deriving (Eq,Show)

data VertexCompSemantic
  = VSPosition
  | VSNormal
  | VSColor
  | VSUV
  | VSCustom Natural
    deriving (Eq,Show)

data VertexComp
  = IntegralComp [Int]
  | UnsignedComp [Word32]
  | FloatingComp [Float]
    deriving (Eq,Ord,Show)

instance Semigroup VertexComp where
  IntegralComp a <> IntegralComp b = IntegralComp $ a ++ b
  UnsignedComp a <> UnsignedComp b = UnsignedComp $ a ++ b
  FloatingComp a <> FloatingComp b = FloatingComp $ a ++ b
  _              <> _              = error "types mismatch"

-- |`Vertices` is a bunch of vertices associated to a `VertexProtocol`.
data Vertices = Vertices {
    -- |Vertex protocol.
    _verticesVProto :: VertexProto
    -- |Vertices.
  , _verticesVerts  :: [Vertex]
  } deriving (Eq,Show)

type Vertex = [VertexComp]
type VertexProto = [VertexCompProto]

makeLenses ''VertexCompProto
makeLenses ''Vertices

-- |Get the integral semantic from a `VertexCompSemantic`.
fromVertexCompSemantic :: VertexCompSemantic -> Natural
fromVertexCompSemantic vcs = case vcs of
    VSPosition -> 0
    VSNormal   -> 1
    VSColor    -> 2
    VSUV       -> 3
    VSCustom s -> 4 + s
 
integral :: [Int] -> VertexComp
integral = IntegralComp

unsigned :: [Word32] -> VertexComp
unsigned = UnsignedComp

floating :: [Float] -> VertexComp
floating = FloatingComp

foldVertexComp :: ([Int] -> a)
               -> ([Word32] -> a)
               -> ([Float] -> a)
               -> VertexComp -> a
foldVertexComp i u f vc = case vc of
    IntegralComp x -> i x
    UnsignedComp x -> u x
    FloatingComp x -> f x

mapVertexComp :: ([Int] -> [Int])
       -> ([Word32] -> [Word32])
       -> ([Float] -> [Float])
       -> VertexComp
       -> VertexComp
mapVertexComp i u f =
    foldVertexComp (integral . i) (unsigned . u) (floating . f)

vertexCompSize :: VertexComp -> Int
vertexCompSize = foldVertexComp length length length

vertexCompBytes :: VertexComp -> Int
vertexCompBytes =
    foldVertexComp
      ((*) (sizeOf (undefined :: Int)) . length)
      ((*) (sizeOf (undefined :: Word32)) . length)
      ((*) (sizeOf (undefined :: Float)) . length)

merge :: Vertex -> Vertex -> Vertex
merge = zipWith (<>)

-- |Deinterleave a list of list of `VertexComp`. A deinterleaved one is simple:
-- a list of `VertexComp` can be something like `[floating a, unsigned b`]. If
-- you have several values like this one and put them in a list, it yields
-- something like:
--
--     [
--       [floating a,unsigned b]
--     , [floating c,unsigned d]
--     ]
--
-- A deinterleaved version of this value joins vertically each part:
--
--     [floating ac,unsigned bd]
--
-- This function may fail if types mismatch.
deinterleave :: [Vertex] -> [VertexComp]
deinterleave = foldl1' merge

-- |Expose a `[VertexComp]` as a C pointer. This function is the only way to
-- poke such lists through *FFI*, so you might want to use `deinterleave` if
-- you want to poke `[[VertexComp]]` (i.e. vertices).
--
-- The function that treats the C pointer takes a `Ptr Word8` that represents
-- the `[VertexComp]` and its size in bytes.
withDeinterleaved :: [VertexComp] -> (Int -> Ptr Word8 -> IO a) -> IO a
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
          let copy x = withArray x $ \bufx -> copyArray p (castPtr bufx)  bytes
          lift $ foldVertexComp copy copy copy c 
