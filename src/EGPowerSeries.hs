-----------------------------------------------------------------------------
--
-- Module      :  EGPowerSeries
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

module EGPowerSeries (
    pullChi,
    egStarGraph,
    egGraph,
    eg2D
) where
    import Polynomials
    import Data.List
    import Complexes

    pullChi :: [Integer] -> Integer -> Integer
    pullChi list chi = genericIndex list chi

    egStarGraph :: Integer -> Integer -> [Integer]
    egStarGraph edges robots = multiplyBrackets (taylorExpand (0-1) edges robots (robots - 2)) [1, 1-edges]

    egGraph :: [Integer] -> Integer -> [Integer]
    egGraph vertices robots =
        multiplyBrackets
            (taylorExpand (0-1) (quot (sum vertices) 2) robots (robots - genericLength(vertices) -1))
            (foldl' multiplyBrackets [1] [[1,1-x]|x<-vertices])

    eg2D :: [Integer] -> [[Integer]] -> [[Integer]] -> Integer -> [Integer]
    eg2D vertices edges faces robots =
        foldl'
            multiplyBrackets
                [1]
                ([[1,1]|x<-faces] ++ [[1,1 - (linkofvertex2D [v] edges faces)]|v<-vertices] ++ [taylorExpand ((linkofedge2D e faces)-1) 1 robots 0|e<-edges])
