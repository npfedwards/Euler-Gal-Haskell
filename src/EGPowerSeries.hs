-----------------------------------------------------------------------------
--
-- Module      :  EGPowerSeries
-- Copyright   :  Nathan Edwards
-- License     :  GPLv3
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
    import IntegerMaths

    pullChi :: [Integer] -> Integer -> Integer -- Takes the nth term of the list (ie, Chi of a poly)
    pullChi list n = genericIndex list n

    egStarGraph :: Integer -> Integer -> [Integer] -- Works out Euler-Gal power series of a star graph
    egStarGraph edges robots = multiplyBrackets (taylorExpand (0-1) edges robots (robots - 2)) [1, 1-edges]

    egGraph :: [Integer] -> Integer -> [Integer] -- Works out the Euler-Gal power series of any graph
    egGraph vertices robots =
        multiplyBrackets
            (taylorExpand (0-1) (division (sum vertices) 2) robots (max (robots - genericLength(vertices) -1) 0))
            (foldl' multiplyBrackets [1] [[1,1-x]|x<-vertices])

    eg2D :: [Integer] -> [[Integer]] -> [[Integer]] -> Integer -> [Integer] -- Works out the Euler-Gal power series of a 2D Simplicial Complex.
    eg2D vertices edges faces robots =
        foldl'
            multiplyBrackets
                [1]
                ([[1,1]|x<-faces] ++ [[1,1 - (linkofvertex2D [v] edges faces)]|v<-vertices] ++ [taylorExpand ((linkofedge2D e faces)-1) 1 robots 0|e<-edges])

    egGeneral :: [[[Integer]]] -> Integer -> [Integer] -- Works out the Euler-Gal power series of a arbitrary simplicial complex
    egGeneral list robots =
        foldl'
            multiplyBrackets
                [1]
                (makeTheList list (tail list) robots)

    makeTheList :: [[[Integer]]] -> [[[Integer]]] -> Integer -> [[Integer]] -- Makes a list of polynomials to be multiplied. 1 per simplex, pertaining to p(t), q(t) as of Euler-Gal.
    makeTheList list remaininglist robots
        | length(remaininglist) == 0                        = [[1]]
        | odd (genericLength(head (head remaininglist)))    = [[1,1 - (linkofnsimplex simplex list)]|simplex<-(head remaininglist)] ++ makeTheList list (tail remaininglist) robots
        | otherwise                                         = [taylorExpand ((linkofnsimplex simplex list)-1) 1 robots 0|simplex<-(head remaininglist)] ++ makeTheList list (tail remaininglist) robots
