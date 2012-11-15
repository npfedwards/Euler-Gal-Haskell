-----------------------------------------------------------------------------
--
-- Module      :  Complexes
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Complexes (
    verticesMake,
    linkofedge2D,
    linkofvertex2D
) where
    import Data.List

    verticesMake :: Integer -> [Integer] --Makes a list of n vertices
    verticesMake n = [0..(n-1)]

    linkofedge2D :: [Integer] -> [[Integer]] -> Integer -- Returns Chi of the star of an edge in 2D
    linkofedge2D edge faces = genericLength(getTheStar edge faces) -- Length of the list of faces containing edge

    getTheStar :: [Integer] -> [[Integer]] -> [[Integer]] -- Finds all nsimplices containing the simplex
    getTheStar simplex nsimplices = filter (flip all simplex . flip elem) nsimplices -- filters the nsimplices that contain all vertices of simplex

    linkofvertex2D :: [Integer] -> [[Integer]] -> [[Integer]] -> Integer -- Returns Chi of the star of a vertex in 2D: V - E
    linkofvertex2D vertex edges faces = genericLength(getTheStar vertex edges) - genericLength(getTheStar vertex faces)

    checkEorF ::[[Integer]] -> [[Integer]] -- Removes duplicates from edges and faces
    checkEorF list = nub (map sort list)

    genVertices :: [[Integer]] -> [[Integer]] -> [Integer]
    genVertices edges faces = nub ((concat $ edges) ++ (concat $ faces))
