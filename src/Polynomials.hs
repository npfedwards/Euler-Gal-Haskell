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
    taylorExpand
) where
    import Combinations

    taylorExpand :: Integer -> Integer -> Integer -> [Integer]
    taylorExpand a b bound
        | bound <= 0        = [0]
        | otherwise         = [(a^x)*(nCr (b+x-1) x)| x <- [0..bound]]


