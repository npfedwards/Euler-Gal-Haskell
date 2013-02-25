-----------------------------------------------------------------------------
--
-- Module      :  Combinations
-- Copyright   :  Nathan Edwards
-- License     :  GPLv3
--
-----------------------------------------------------------------------------

module Combinations (
    nCr,
    nPr,
    factorial
) where
    import FoldPrime as Fold
    import IntegerMaths

    nPr :: Integer -> Integer -> Integer -- n Permute r
    nPr 0 r = 1
    nPr n r = foldl' (*) 1 [n-r+1..n] -- folds the list [n-r+1, ... , n] by multiplication

    nCr :: Integer -> Integer -> Integer -- n Choose r
    nCr n r
        | n == 0        = 1
        | r == 1        = n
        | r == n - 1    = n
        | otherwise     = division (nPr n r) (factorial r) -- quotients nPr by r!

    factorial :: Integer -> Integer
    factorial n = foldl' (*) 1 [1..n] -- folds the list [1, ... , n] by multiplication
