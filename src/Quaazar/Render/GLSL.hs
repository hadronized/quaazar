-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
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
declUniform :: Natural -> String -> String
declUniform s n = "layout (location = " ++ show s ++ ") uniform " ++ n ++ ";"

camProjViewSem :: Natural 
camProjViewSem = 0

modelSem :: Natural 
modelSem = 1

eyeSem :: Natural 
eyeSem = 2

ligAmbColSem :: Natural 
ligAmbColSem = 3

ligAmbPowSem :: Natural 
ligAmbPowSem = 4

ligOmniNbSem :: Natural 
ligOmniNbSem = 5

-- Since there’re 6 matrices, they take locations 6, 7, 8, 9, 10 and 11.
ligProjViewsSem :: Natural
ligProjViewsSem = 6

ligPosSem :: Natural
ligPosSem = 12

ligRadSem :: Natural
ligRadSem = 13

ligIRadSem :: Natural
ligIRadSem = 14

shadowmapIndexSem :: Natural
shadowmapIndexSem = 15

--------------------------------------------------------------------------------
-- GLSL BINDING POINTS
declUniformBlock :: Natural -> String -> String
declUniformBlock bp block = "layout (std430,binding = " ++ show bp ++ ") buffer " ++ block ++ ";"

ligOmniSSBOBP :: Natural
ligOmniSSBOBP = 0
