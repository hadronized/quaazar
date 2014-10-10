#############################################################################
##
## Copyright   : (C) 2014 Dimitri Sabadie
## License     : BSD3
##
## Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
##
############################################################################

bl_info = {
    "name"     : "Export mesh to photon JSON (.ymsh)"
  , "author"   : "Dimitri Sabadie"
  , "category" : "Import-Export"
  , "location" : "File > Import-Export"
}

import bpy
from bpy_extras.io_utils import ExportHelper
from bpy.props import BoolProperty
import json

class PhotonMeshExporter(bpy.types.Operator, ExportHelper):
  """Photon Mesh Exporter Script"""
  bl_idname      = "object.photon_mesh_exporter"
  bl_label       = "Photon Mesh Exporter"
  bl_description = "Export all meshes from the scene into a directory"
  bl_options     = {'REGISTER'}

  filename_ext   = ".ymsh"

  sparse = BoolProperty (
      name        = "Sparse output"
    , description = "Should the output file be sparse?"
    , default     = False
    , )

  def execute(self, context):
    print("-- ----------------------- --")
    print("-- Photon Mesh JSON Export --")
    o = bpy.context.active_object
    if o == None:
      print("E: no mesh selected")
    else:
      msh = o.data
      if not hasOnlyTris(msh):
        print("W: '" + msh.name + "' is not elegible to export, please convert quadrangles to triangles")
      else:
        print("I: exporting '" + msh.name + "'")
        phmsh = toPhotonMesh(msh)
        fp = open(self.filepath, "w")
        fp.write(phmsh.toJSON(self.sparse))
        fp.close()
    print("-- ----------------------- --")
    return {'FINISHED'}

def register():
  bpy.utils.register_class(PhotonMeshExporter)

def unregister():
  bpy.utils.unregister_class(PhotonMeshExporter)

if __name__ == "__main__":
  register()

class PhotonMesh:
  def __init__(self, vs, vgr):
    self.vertices = vs
    self.vgroup   = vgr

  def toJSON(self, sparse):
    i = 2 if sparse else None
    d = { "vertices" : {"interleaved" : True, "values" : self.vertices}, "vgroup" : { "grouping" : "triangles", "triangles" : self.vgroup }
    return json.dumps(d, sort_keys=True, indent=i)

def hasOnlyTris(msh):
  for poly in msh.polygons:
    if len(poly.vertices) > 3:
      return False
  return True

def toPhotonMesh(msh):
  vs = []
  vg = []

  # vertices
  for vert in msh.vertices:
    pos = [round_(vert.co[0]),round_(vert.co[1]),round_(vert.co[2])]
    nor = [round_(vert.normal[0]),round_(vert.normal[1]),round_(vert.normal[2])]
    vs.append([pos,nor])

  # vertex group
  i = 0
  ll = len(msh.loops)
  while i < ll:
    vg.append([ msh.loops[i].vertex_index
              , msh.loops[i+1].vertex_index
              , msh.loops[i+2].vertex_index
              ])
    i += 3

  return PhotonMesh(vs,vg)

def round_(x):
  return round(x,6)
