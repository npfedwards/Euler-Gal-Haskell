-----------------------------------------------------------------------------
--
-- Module      :  Combinations
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

module Combinations (
    nCr,
    nPr,
    factorial
) where
    import FoldPrime as Fold

    nPr :: Integer -> Integer -> Integer -- n Permute r
    nPr 0 r = 1
    nPr n r = foldl' (*) 1 [n-r+1..n] -- folds the list [n-r+1, ... , n] by multiplication

    nCr :: Integer -> Integer -> Integer -- n Choose r
    nCr 0 r = 1
    nCr n r = quot (nPr n r) (factorial r) -- quotients nPr by r!

    factorial :: Integer -> Integer
    factorial n = foldl' (*) 1 [1..n] -- folds the list [1, ... , n] by multiplication
