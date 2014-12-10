-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Photon.Render.Mesh where

import Control.Lens
import Control.Monad.Trans ( MonadIO(..) )
import Data.Word ( Word32 )
import Foreign.Ptr ( nullPtr )
import Foreign.Storable ( sizeOf )
import Linear ( V2, V3 )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Photon.Core.Entity ( Entity )
import Photon.Core.Mesh hiding ( Line, Triangle )
import Photon.Core.Normal
import Photon.Core.Position
import Photon.Core.UV
import Photon.Render.GL.Buffer
import Photon.Render.GL.Entity ( entityTransform )
import Photon.Render.GL.Primitive
import Photon.Render.GL.Shader ( (@?=) )
import Photon.Render.GL.VertexArray
import Photon.Render.Semantics ( modelMatrixSem )
import Photon.Render.Shader ( GPUProgram, programSemantic )

data GPUMesh = GPUMesh {
    -- |VBO.
    _gpuMeshVBO    :: Buffer
    -- |IBO.
  , _gpuMeshIBO    :: Buffer
    -- |VAO.
  , _gpuMeshVAO    :: VertexArray
    -- |Primitive.
  , _gpuMeshPrim   :: Primitive
    -- |Vertices number.
  , _gpuMeshVertNB :: Int
  } deriving (Eq,Show)

makeLenses ''GPUMesh

-- |OpenGL 'Mesh' representation.
gpuMesh :: (MonadIO m) => Mesh -> m GPUMesh
gpuMesh msh = liftIO $ case msh^.meshVertices of
    Interleaved v -> gpuMesh (msh & meshVertices .~ deinterleaved v)
    Deinterleaved vnb positions normals uvs -> do
      vb <- genBuffer
      ib <- genBuffer
      va <- genVertexArray

      -- VBO
      let
        vbytes      = posBytes + normalBytes + uvsBytes
        posBytes    = fromIntegral vnb * sizeOf (undefined :: V3 Float)
        normalBytes = fromIntegral vnb * sizeOf (undefined :: V3 Float)
        uvsBytes    = fromIntegral vnb * uvnb * sizeOf (undefined :: V2 Float)
        uvnb        = if null uvs then 0 else length (head uvs)

      bindBuffer vb ArrayBuffer
      initBuffer ArrayBuffer (fromIntegral vbytes)
      bufferSubData ArrayBuffer 0 (fromIntegral posBytes) (map unPosition positions)
      bufferSubData ArrayBuffer posBytes (fromIntegral normalBytes) (map unNormal normals)
      bufferSubData ArrayBuffer (fromIntegral $ posBytes + normalBytes) (fromIntegral uvsBytes) (concatMap (map unUV) uvs)
      unbindBuffer ArrayBuffer

      -- IBO
      let ibytes = fromIntegral $ verticesNb * sizeOf (undefined :: Word32)

      bindBuffer ib IndexBuffer
      initBuffer IndexBuffer ibytes
      bufferSubData IndexBuffer 0 ibytes inds
      unbindBuffer IndexBuffer

      -- VAO
      bindVertexArray va
      bindBuffer vb ArrayBuffer
      
      -- position
      enableVertexAttrib (fromIntegral positionAttribute)
      vertexAttribPointer (fromIntegral positionAttribute) 3 Floats False 0
      -- normal
      enableVertexAttrib (fromIntegral normalAttribute)
      vertexAttribPointer (fromIntegral normalAttribute) 3 Floats True posBytes
      -- uv 0 -- TODO: support all uvchans
      enableVertexAttrib (fromIntegral uv0Attribute)
      vertexAttribPointer (fromIntegral uv0Attribute) 2 Floats True (posBytes + normalBytes)

      unbindBuffer ArrayBuffer

      bindBuffer ib IndexBuffer
      unbindVertexArray

      unbindBuffer IndexBuffer

      return $ GPUMesh vb ib va prim verticesNb
  where
    inds          = msh^.meshVGroup.to fromVGroup
    verticesNb    = length inds
    prim          = toGLPrimitive (msh^.meshVGroup)

renderMesh :: (MonadIO m) => GPUProgram -> GPUMesh -> Entity -> m ()
renderMesh program msh ent = liftIO $ do
    sem modelMatrixSem @?= entityTransform ent
    bindVertexArray (msh^.gpuMeshVAO)
    glDrawElements (fromPrimitive $ msh^.gpuMeshPrim) vnb gl_UNSIGNED_INT nullPtr
  where
    sem = programSemantic program
    vnb = fromIntegral (msh^.gpuMeshVertNB)

toGLPrimitive :: VGroup -> Primitive
toGLPrimitive vg = case vg of
    Points{}     -> Point
    Lines{}      -> Line
    Triangles{}  -> Triangle
    SLines{}     -> SLine
    STriangles{} -> STriangle

positionAttribute :: Natural
positionAttribute = 0

normalAttribute :: Natural
normalAttribute = 1

uv0Attribute :: Natural
uv0Attribute = 2
