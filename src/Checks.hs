-----------------------------------------------------------------------------
--
-- Module      :  Checks
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

module Checks (
    starCheck,
    starChecks,
    oneRobot
) where
    import Combinations
    import IntegerMaths
    import FoldPrime
    import EGPowerSeries
    import Data.List

    fStar :: Integer -> Integer -> Integer -- Ordered case for a star of order mu with n robots
    fStar mu n = 0 - ((nPr (mu + n - 2) (n - 1)) * (((n - 1) * mu) - (2 * n) + 1))

    starCheck :: Integer -> Integer -> Integer -- Unordered case for above star
    starCheck mu n = division (fStar mu n) (factorial n)

    starChecks :: Integer -> [[Bool]] -- Returns the boolean expression dependent on whether the members of the list of star checks and egStarGraph are equal
    starChecks n = [zipWith (==) (listegs x n) (liststar x n) | x <- [1..n]]

    listegs :: Integer -> Integer -> [Integer] -- Returns a list of outputs for egStarGraph
    listegs e n = [pullChi (egStarGraphOutput) x | x <- [1..n]]
        where egStarGraphOutput = egStarGraph e n

    listeg :: [Integer] -> Integer -> [Integer] --Returns a list of outputs for egGraph
    listeg v n = [pullChi (egGraphOutput) x | x <- [1..n]]
        where egGraphOutput = egGraph v n

    liststar :: Integer -> Integer -> [Integer] -- Returns a list of star checks
    liststar e n = [starCheck e x | x <- [1..n]]

    oneRobot :: Integer -> [[Integer]] -> [[Integer]] -> Integer -- Takes #vertices, list of edges and robots and returns the euler characteristic where number of robots = 1
    oneRobot v e f = v - genericLength(e) + genericLength(f)
