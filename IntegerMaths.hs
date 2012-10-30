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
    division
) where
    division :: Integer -> Integer -> Integer
    division 0 den = 0
    division num 0 = 0 -- Just define it as 0 to catch errors.
    division num den = (quot num den) + division (remainder num den) den
        where remainder num den = num - ((quot num den) * den)

