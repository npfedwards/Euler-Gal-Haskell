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
    verticesMake,
    linkofedge2D,
    linkofvertex2D
) where
    import Data.List

    verticesMake :: Integer -> [Integer]
    verticesMake n = [0..(n-1)]

    linkofedge2D :: [Integer] -> [[Integer]] -> Integer
    linkofedge2D edge faces = genericLength(getTheStar edge faces)

    getTheStar :: [Integer] -> [[Integer]] -> [[Integer]]
    getTheStar simplex nsimplices = filter (flip all simplex . flip elem) nsimplices

    linkofvertex2D :: [Integer] -> [[Integer]] -> [[Integer]] -> Integer
    linkofvertex2D vertex edges faces = genericLength(getTheStar vertex edges) - genericLength(getTheStar vertex faces)
