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
    nPr
) where
    import FoldPrime as Fold
    nPr :: Integer -> Integer -> Integer
    nPr 0 r = 1
    nPr n r = foldl' (*) 1 [n-r+1..n]

    nCr :: Integer -> Integer -> Integer
    nCr 0 r = 1
    nCr n r = quot (nPr n r) (foldl' (*) 1 [1..r])
