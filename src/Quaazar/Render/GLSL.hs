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

module Quaazar.Render.GLSL where

import Numeric.Natural ( Natural )

--------------------------------------------------------------------------------
-- GLSL SEMANTICS
declUniform :: Int -> String -> String
declUniform s n = "layout (location = " ++ show s ++ ") uniform " ++ n ++ ";"

camProjViewSem :: Int
camProjViewSem = 0

modelSem :: Int
modelSem = 1

eyeSem :: Int
eyeSem = 2

matDiffAlbSem :: Int
matDiffAlbSem = 3

matSpecAlbSem :: Int
matSpecAlbSem = 4

matShnSem :: Int
matShnSem = 5

ligAmbColSem :: Int
ligAmbColSem = 6

ligAmbPowSem :: Int
ligAmbPowSem = 7

ligOmniNbSem :: Int
ligOmniNbSem = 8

--------------------------------------------------------------------------------
-- GLSL BINDING POINTS
declUniformBlock :: Natural -> String -> String
declUniformBlock bp block = "layout (std430,binding = " ++ show bp ++ ") buffer " ++ block ++ ";"

ligOmniSSBOBP :: Natural
ligOmniSSBOBP = 0
