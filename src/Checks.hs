-----------------------------------------------------------------------------
--
-- Module      :  Checks
-- Copyright   :  Nathan Edwards
-- License     :  Creative Commons Attribution ShareAlike 3.0
--
-- This module contains functions to check whether the results given by the EGPowerSeries functions are accurate.
--
-----------------------------------------------------------------------------

module Checks (
    starCheck,
    starChecks,
    oneRobot,
    kPage,
    manifold
) where
    import Combinations
    import IntegerMaths
    import FoldPrime as Fp
    import EGPowerSeries
    import Data.List
    import Polynomials

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

    kPage :: Integer -> Integer -> Integer
    kPage k r = pullChi (Fp.foldl'
                            multiplyBrackets
                            [1]
                            ([[1,1]|x<-[1..k]] ++ [taylorExpand (k-1) 1 r 0])) r

    manifold :: Integer -> Integer -> Integer -> Integer -- Uses Theorem 2.5 (Farber) to find eu_X for a manifold.
    manifold chi dim r
        |   odd dim     =   pullChi (taylorExpand (0-1) chi r (r-1)) r
        |   otherwise   =   pullChi (Fp.foldl' multiplyBrackets [1] [[1,1]|x<-[1..chi]]) r



