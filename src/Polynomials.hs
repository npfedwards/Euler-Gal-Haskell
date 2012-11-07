-----------------------------------------------------------------------------
--
-- Module      :  Polynomials
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

module Polynomials (
    taylorExpand,
    multiplyBrackets
) where
    import Combinations
    import FoldPrime
    import Data.List

    taylorExpand :: Integer -> Integer -> Integer -> Integer -> [Integer]
    taylorExpand a b bound lowerbound -- expand (1-ax)^b as far as x^bound
        | bound < 0         = [0]
        | b == 0            = [1]
        | otherwise         = [0|x<-[0..(lowerbound-1)]]++[(a^x)*(nCr (b+x-1) x)| x <- [lowerbound..bound]]

    multiplyBrackets :: [Integer] -> [Integer] -> [Integer]
    multiplyBrackets as bs = addLists (listMultiplications as bs)

    listMultiplications :: [Integer] -> [Integer] -> [[Integer]]
    listMultiplications [] bs = [[0]]
    listMultiplications as [] = [[0]]
    listMultiplications as bs = [map (* (head bs)) as] ++ listMultiplications as (tail bs)

    addLists = map sum . transpose . zipWith (++) (inits (repeat 0))
