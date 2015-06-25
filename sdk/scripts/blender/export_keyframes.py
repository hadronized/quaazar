#############################################################################
##
## Copyright   : (C) 2015 Dimitri Sabadie
## License     : BSD3
##
## Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
##
############################################################################

bl_info = {
    "name"     : "Export keyframes to Quaazar JSON (.qkfr)"
  , "author"   : "Dimitri Sabadie"
  , "category" : "Import-Export"
  , "location" : "File > Import-Export"
}

import bpy
import mathutils
from bpy_extras.io_utils import ExportHelper
from bpy.props import BoolProperty
from math import pi
import json

def round_(x):
  return round(x,6)

class QuaazarKeyframesExporter(bpy.types.Operator, ExportHelper):
  """Quaazar Keyframes Exporter Script"""
  bl_idname      = "object.quaazar_keyframes_exporter"
  bl_label       = "Quaazar Keyframes Exporter"
  bl_description = "Export all keyframes from the scene"
  bl_options     = {'REGISTER'}

  filename_ext   = ".qkfr"

  sparse = BoolProperty (
      name        = "Sparse output"
    , description = "Should the output file be sparse?"
    , default     = False
    , )

  def execute(self, context):
    print("-- Quaazar Keyframes JSON Export --")
    o = bpy.context.active_object
    print("-- ----------------------- --")
    return {'FINISHED'}

def register():
  bpy.utils.register_class(QuaazarMeshExporter)

def unregister():
  bpy.utils.unregister_class(QuaazarMeshExporter)

if __name__ == "__main__":
  register()

