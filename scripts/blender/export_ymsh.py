#############################################################################
##
## Copyright   : (C) 2014 Dimitri Sabadie
## License     : BSD3
##
## Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
## Portability : portable
##
############################################################################

bl_info = {
    "name": "Export mesh to photon JSON (.ymsh)"
  , "author": "Dimitri Sabadie"
  , "category": "Import-Export"
  , "location": "File > Import-Export"
}

import bpy

class PhotonMeshExporter(bpy.types.Operator):
  """Photon Mesh Exporter Script"""
  bl_idname      = "object.photon_mesh_exporter"
  bl_label       = "Photon Mesh Exporter"
  bl_description = "Export all meshes from the scene into a directory"
  bl_options     = {'REGISTER','UNDO'}

  def execute(self, context):
    print("hello world!")
    return {'FINISHED'}

def register():
  bpy.utils.register_class(MoveObjectX)

def unregister():
  bpy.utils.unregister_class(MoveObjectX)

if __name__ == "__main__":
  register()

class PhotonMesh:
    def __init__(self):
        self.vertices = []
        self.vgroup   = {}

def getSceneMeshes():
  return [obj for obj in bpy.context.scene.objects if obj.type == 'MESH']
