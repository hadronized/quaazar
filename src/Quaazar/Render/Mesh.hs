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

module Quaazar.Render.Mesh where

import Control.Lens
import Control.Monad.Trans ( MonadIO(..) )
import Data.Word ( Word32 )
import Foreign.Ptr ( nullPtr )
import Foreign.Storable ( sizeOf )
import Linear ( M44, V2, V3 )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Core.Entity ( Entity )
import Quaazar.Core.Mesh hiding ( Line, Triangle )
import Quaazar.Core.Normal
import Quaazar.Core.Position
import Quaazar.Core.UV
import Quaazar.Render.GL.Buffer
import Quaazar.Render.GL.GLObject
import Quaazar.Render.GL.Entity ( entityTransform )
import Quaazar.Render.GL.Primitive
import Quaazar.Render.GL.Shader ( Uniform, (@=) )
import Quaazar.Render.GL.VertexArray
import Quaazar.Utils.Scoped

data GPUMesh = GPUMesh {
    vertexBuffer :: Buffer
  , indexBuffer  :: Buffer
  , renderMesh   :: Uniform (M44 Float)
                 -> Entity
                 -> IO ()
  }

makeLenses ''GPUMesh

-- |OpenGL 'Mesh' representation.
gpuMesh :: (MonadScoped IO m,MonadIO m) => Mesh -> m GPUMesh
gpuMesh msh = case msh^.meshVertices of
    Interleaved v -> gpuMesh (msh & meshVertices .~ deinterleaved v)
    Deinterleaved vnb positions normals uvs -> do
      [vb,ib] <- genObjects 2
      va <- genObject

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

      return . GPUMesh vb ib $ \modelSem ent -> do
        modelSem @= entityTransform ent
        bindVertexArray va
        glDrawElements (fromPrimitive prim) (fromIntegral verticesNb) gl_UNSIGNED_INT nullPtr
  where
    inds          = msh^.meshVGroup.to fromVGroup
    verticesNb    = length inds
    prim          = toGLPrimitive (msh^.meshVGroup)

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