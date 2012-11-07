-----------------------------------------------------------------------------
--
-- Module      :  Complexes
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

module Complexes (
    simplices0,
    linkofedge2D,
) where
    import Data.List
    simplices0 :: Integer -> [Integer]
    simplices0 n = [0..(n-1)]

    linkofedge2D :: [Integer] -> [[Integer]] -> Integer
    linkofedge2D edge faces = genericLength(filter (flip all edge . flip elem) faces)
