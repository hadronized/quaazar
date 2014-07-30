{-# LANGUAGE TypeFamilies #-}

module Photon.Core.Interpreter where

import Photon.Core.Entity ( Entities )

-- |Typeclass for scene interpreters. `Interpretation` is the result of the
-- interpretation process, and is interpreter-dependent – type family.
--
-- You interpret a scene with the `interpret` function.
class Interpreter q a where
  -- |Scene interpretation type family.
  type Interpretation :: *
  -- |Interpret a scene. You may want to use a curried version in order to
  -- generate internal representations and speed things up. Of course, the
  -- interpreter generation is interpreter dependent. For some ones it doesn’t
  -- require nothing (pure code), others might need `IO` and so on and so
  -- forth.
  interpret :: q a -> Entities a -> Interpretation
