-----------------------------------------------------------------------------
--
-- Module      :  Complexes
-- Copyright   :  Nathan Edwards
-- License     :  Creative Commons Attribution ShareAlike 3.0
--
-- Functions for creating data structures of complexes and finding the links of simplices
--
-----------------------------------------------------------------------------

module Complexes (
    verticesMake,
    linkofedge2D,
    linkofvertex2D,
    checkEorF,
    genVertices,
    genSimplices,
    linkofnsimplex,
    genAll,
    cleanList
) where
    import Data.List
    import IntegerMaths

    verticesMake :: Integer -> [Integer] --Makes a list of n vertices
    verticesMake n = [0..(n-1)]

    linkofedge2D :: [Integer] -> [[Integer]] -> Integer -- Returns Chi of the star of an edge in 2D
    linkofedge2D edge faces = genericLength(getTheStar edge faces) -- Length of the list of faces containing edge

    getTheStar :: [Integer] -> [[Integer]] -> [[Integer]] -- Finds all nsimplices containing the simplex
    getTheStar simplex nsimplices = filter (flip all simplex . flip elem) nsimplices -- filters the nsimplices that contain all vertices of simplex

    linkofvertex2D :: [Integer] -> [[Integer]] -> [[Integer]] -> Integer -- Returns Chi of the star of a vertex in 2D: V - E
    linkofvertex2D vertex edges faces = genericLength(getTheStar vertex edges) - genericLength(getTheStar vertex faces)

    checkEorF ::[[Integer]] -> [[Integer]] -- Removes duplicates from edges and faces
    checkEorF list = nub (map sort (removeLoops list))

    genVertices :: [[Integer]] -> [[Integer]] -> [Integer] -- generates a list of vertices based on the definitions of edges and faces
    genVertices edges faces = nub ((concat $ edges) ++ (concat $ faces))

    removeLoops :: [[Integer]] -> [[Integer]] -- Removes loops ie [2,2] or [1,1,3]. Currently just removes them entirely.
    removeLoops list = filter noDuplicates list

    noDuplicates :: [Integer] -> Bool
    noDuplicates list = list == nub list

    genSimplices :: [[Integer]] -> Integer -> [[Integer]] -- Takes a list of m-simplices (m>n) and returns all n-simplices
    genSimplices list n = filter (orderIs n) (concat' $ map subsequences list)

    concat' :: [[[Integer]]] -> [[Integer]]
    concat' = foldr (++) [[]]

    orderIs :: Integer -> [Integer] -> Bool
    orderIs n list = genericLength list == n

    linkofnsimplex :: [Integer] -> [[[Integer]]] -> Integer --NOTE: doesn't work if complex not in order
    linkofnsimplex simplex listofsimplices
        | odd (genericLength simplex)     = asum (map genericLength (map (getTheStar simplex) listofsimplices)) + 1
        | otherwise                      = 1 - asum (map genericLength (map (getTheStar simplex) listofsimplices))

    genAll :: [[Integer]] -> Integer -> Integer -> [[[Integer]]] -> [[[Integer]]]
    genAll list m n newlist
        | m > n     = newlist
        | otherwise = genAll list (m + 1) n (newlist ++ [genSimplices list m])

    cleanList :: [[[Integer]]] -> [[[Integer]]] -- Still needs to remove duplicates
    cleanList list = map nub (map (map sort) list)
