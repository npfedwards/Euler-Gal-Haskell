-----------------------------------------------------------------------------
--
-- Module      :  EGPowerSeries
-- Copyright   :  Nathan Edwards
-- License     :  Creative Commons Attribution ShareAlike 3.0
--
-- Computations of the Euler Gal Power Series for a configuration space
--
-----------------------------------------------------------------------------

module EGPowerSeries (
    pullChi,
    egStarGraph,
    egGraph,
    eg2D,
    egGeneral,
    makeTheList
) where
    import Polynomials
    import Data.List
    import Complexes

    pullChi :: [Integer] -> Integer -> Integer -- Takes the nth term of the list (ie, Chi of a poly)
    pullChi list n = genericIndex list n

    egStarGraph :: Integer -> Integer -> [Integer] -- Works out Euler-Gal power series of a star graph
    egStarGraph edges robots = multiplyBrackets (taylorExpand (0-1) edges robots (robots - 2)) [1, 1-edges]

    egGraph :: [Integer] -> Integer -> [Integer] -- Works out the Euler-Gal power series of any graph
    egGraph vertices robots =
        multiplyBrackets
            (taylorExpand (0-1) (quot (sum vertices) 2) robots (robots - genericLength(vertices) -1))
            (foldl' multiplyBrackets [1] [[1,1-x]|x<-vertices])

    eg2D :: [Integer] -> [[Integer]] -> [[Integer]] -> Integer -> [Integer] -- Works out the Euler-Gal power series of a 2D Simplicial Complex.
    eg2D vertices edges faces robots =
        foldl'
            multiplyBrackets
                [1]
                ([[1,1]|x<-faces] ++ [[1,1 - (linkofvertex2D [v] edges faces)]|v<-vertices] ++ [taylorExpand ((linkofedge2D e faces)-1) 1 robots 0|e<-edges])

    egGeneral :: [[[Integer]]] -> Integer -> [Integer]
    egGeneral list robots =
        foldl'
            multiplyBrackets
                [1]
                (makeTheList list (tail list) robots)

    makeTheList :: [[[Integer]]] -> [[[Integer]]] -> Integer -> [[Integer]]
    makeTheList list [[[]]] robots = []
    makeTheList list remaininglist robots =
        [[1,1 - (linkofnsimplex simplex list)]|simplex<-(head remaininglist)] ++ [taylorExpand ((linkofnsimplex simplex list)-1) 1 robots 0|simplex<-head (tail remaininglist)] ++ makeTheList list (tail (tail remaininglist)) robots
