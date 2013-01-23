-----------------------------------------------------------------------------
--
-- Module      :  Polynomials
-- Copyright   :  Nathan Edwards
-- License     :  Creative Commons Attribution ShareAlike 3.0
--
-- Manipulation of an polynomials/power series
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
    taylorExpand a b bound lowerbound -- expand (1+ax)^-b as far as x^bound
        | bound < 0         = [0]
        | b == 0            = [1]
        | a == 0            = [1]
        | b == 1            = [0|x<-[0..(lowerbound-1)]]++[((0-a)^x)| x <- [lowerbound..bound]]
        | otherwise         = [0|x<-[0..(lowerbound-1)]]++[((0-a)^x)*(nCr (b+x-1) x)| x <- [lowerbound..bound]]

    multiplyBrackets :: [Integer] -> [Integer] -> [Integer] -- Multplies two lists as if they are ordered lists of the coefficients of a polynomial.
    multiplyBrackets as [0] = [0]
    multiplyBrackets as [1] = as
    multiplyBrackets [0] bs = [0]
    multiplyBrackets [1] bs = bs
    multiplyBrackets as bs = addLists (listMultiplications as bs)

    listMultiplications :: [Integer] -> [Integer] -> [[Integer]]
    listMultiplications [] bs = [[0]]
    listMultiplications as [] = [[0]]
    listMultiplications as bs = [map (* (head bs)) as] ++ listMultiplications as (tail bs)

    addLists = map sum . transpose . zipWith (++) (inits (repeat 0))
