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
    import Data.List (transpose)

    taylorExpand :: Integer -> Integer -> Integer -> [Integer]
    taylorExpand a b bound -- expand (1-ax)^b as far as x^bound
        | bound < 0         = [0]
        | b == 0            = [1]
        | otherwise         = [(a^x)*(nCr (b+x-1) x)| x <- [0..bound]]

    multiplyBrackets :: [Integer] -> [Integer] -> [Integer]
    multiplyBrackets as bs = foldl' (zipWithMore (+)) [] (postponeLists (listMultiplications as bs))

    listMultiplications :: [Integer] -> [Integer] -> [[Integer]]
    listMultiplications [] bs = [[0]]
    listMultiplications as [] = [[0]]
    listMultiplications as bs = [map (* (head bs)) as] ++ listMultiplications as (tail bs)

    postponeLists [] = []
    postponeLists (l:ls) = l : map (0:) (postponeLists ls)

    zipWithMore :: (a -> a -> a) -> [a] -> [a] -> [a]
    zipWithMore f (a:as) (b:bs) = f a b : zipWithMore f as bs
    zipWithMore f []      bs      = bs
    zipWithMore f as      []      = as

    addLists :: Num a => [[a]] -> [a]
    addLists xs = map sum . transpose $ zipWith (\n x -> replicate n 0 ++ x) [0..] xs
