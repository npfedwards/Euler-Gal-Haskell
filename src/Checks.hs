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
    starChecks
) where
    import Combinations
    import IntegerMaths
    import FoldPrime
    import EGPowerSeries

    fStar :: Integer -> Integer -> Integer
    fStar mu n = 0 - ((nPr (mu + n - 2) (n - 1)) * (((n - 1) * mu) - (2 * n) + 1))

    starCheck :: Integer -> Integer -> Integer
    starCheck mu n = division (fStar mu n) (factorial n)

    starChecks :: Integer -> [[Bool]]
    starChecks n = [zipWith (==) (listeg x n) (liststar x n) | x <- [1..n]]

    listeg :: Integer -> Integer -> [Integer]
    listeg e n = [pullChi (egStarGraphOutput) x | x <- [1..n]]
        where egStarGraphOutput = egStarGraph e n

    liststar :: Integer -> Integer -> [Integer]
    liststar e n = [starCheck e x | x <- [1..n]]
