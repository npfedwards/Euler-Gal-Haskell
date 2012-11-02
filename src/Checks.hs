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

    starChecks :: Integer -> [Bool]
    starChecks n = zipWith (==) (listeg n) (liststar n)

    listeg :: Integer -> [Integer]
    listeg n = [pullChi (egStarGraph (x) n) n | x <- [1..100]]

    liststar :: Integer -> [Integer]
    liststar n = [starCheck (x) n | x <- [1..100]]
