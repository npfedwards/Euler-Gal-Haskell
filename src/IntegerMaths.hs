-----------------------------------------------------------------------------
--
-- Module      :  IntegerMaths
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

module IntegerMaths (
    division,
    asum
) where
    division :: Integer -> Integer -> Integer
    division 0 den = 0
    division num 0 = 0 -- Just define it as 0 to catch errors.
    division num den = (quot num den) + division (remainder num den) den
        where remainder num den = num - ((quot num den) * den)

    asum :: [Integer] -> String -> Integer
    asum [] neg = 0
    asum x:xs "n" = x - (asum xs "p")
    asum x:xs "p" = x + (asum xs "n")
    asum x:xs neg = x + (asum xs "n")
