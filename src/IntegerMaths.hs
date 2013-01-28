-----------------------------------------------------------------------------
--
-- Module      :  IntegerMaths
-- Copyright   :  Nathan Edwards
-- License     :  GPLv3
--
-- Integer division and an alternating sum
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

    asum :: [Integer] -> Integer
    asum [] = 0
    asum (x:xs) = x - (asum xs)
