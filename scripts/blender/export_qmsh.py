#############################################################################
##
## Copyright   : (C) 2015 Dimitri Sabadie
## License     : BSD3
##
## Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
##
############################################################################

bl_info = {
    "name"     : "Export mesh to Quaazar JSON (.qmsh)"
  , "author"   : "Dimitri Sabadie"
  , "category" : "Import-Export"
  , "location" : "File > Import-Export"
}

import bpy
from bpy_extras.io_utils import ExportHelper
from bpy.props import BoolProperty
import json

class QuaazarMeshExporter(bpy.types.Operator, ExportHelper):
  """Quaazar Mesh Exporter Script"""
  bl_idname      = "object.quaazar_mesh_exporter"
  bl_label       = "Quaazar Mesh Exporter"
  bl_description = "Export all meshes from the scene into a directory"
  bl_options     = {'REGISTER'}

  filename_ext   = ".qmsh"

  sparse = BoolProperty (
      name        = "Sparse output"
    , description = "Should the output file be sparse?"
    , default     = False
    , )

  smoothNormals = BoolProperty (
      name        = "Smooth normals"
    , description = "Smooth normals?"
    , default     = True
    , )

  def execute(self, context):
    print("-- ----------------------- --")
    print("-- Quaazar Mesh JSON Export --")
    o = bpy.context.active_object
    if o == None:
      print("E: no mesh selected")
    else:
      msh = o.data
      if not hasOnlyTris(msh):
        print("W: '" + msh.name + "' is not elegible to export, please convert quadrangles to triangles")
      else:
        print("I: exporting '" + msh.name + "'")
        phmsh = toQuaazarMesh(msh, self.smoothNormals)
        fp = open(self.filepath, "w")
        fp.write(phmsh.toJSON(self.sparse))
        fp.close()
    print("-- ----------------------- --")
    return {'FINISHED'}

def register():
  bpy.utils.register_class(QuaazarMeshExporter)

def unregister():
  bpy.utils.unregister_class(QuaazarMeshExporter)

if __name__ == "__main__":
  register()

class QuaazarMesh:
  def __init__(self, vs, vgr):
    self.vertices = vs
    self.vgroup   = vgr

  def toJSON(self, sparse):
    i = 2 if sparse else None
    d = { "vertices" : {"interleaved" : True, "values" : self.vertices}, "vgroup" : { "grouping" : "triangles", "triangles" : self.vgroup } }
    return json.dumps(d, sort_keys=True, indent=i)

# Check whether a mesh has only triangles.
def hasOnlyTris(msh):
  for poly in msh.polygons:
    if len(poly.vertices) > 3:
      return False
  return True

# Build a vertex.
def createVertex(vertices, vertID, smoothNormals):
  vert = vertices[vertID]
  co = [round_(vert.co[0]),round_(vert.co[1]),round_(vert.co[2])]
  no = []
  if smoothNormals:
    nor = [round_(vert.normal[0]),round_(vert.normal[1]),round_(vert.normal[2])]
  return [co,no,[]]

# Look for a vertex. If it exists, return its ID. Otherwise, return None.
def lookupVertex(vertices, vert):
  try:
    return vertices.index(vert)
  except ValueError:
    return None
    
# Record a new vertex for vertices / indices.
def recordVertex(vertices, indices, vert):
  vid = lookupVertex(vertices, vert)

  if vid == None:
    vid = len(vertices)
    vertices.append(vert)

  return vid
  
def toQuaazarMesh(msh, smoothNormals):
  i = 0
  vnb = len(msh.loops)
  vertices = []
  indices = []
  while i < vnb:
    a = createVertex(msh.vertices, msh.loops[i].vertex_index, smoothNormals)
    b = createVertex(msh.vertices, msh.loops[i+1].vertex_index, smoothNormals)
    c = createVertex(msh.vertices, msh.loops[i+2].vertex_index, smoothNormals)
    aID = recordVertex(vertices, indices, a)
    bID = recordVertex(vertices, indices, b)
    cID = recordVertex(vertices, indices, c)
    indices.append([aID,bID,cID])

    i += 3

  return QuaazarMesh(vertices,indices)

def round_(x):
  return round(x,6)
