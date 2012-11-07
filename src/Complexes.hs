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
    simplices0 :: Integer -> [Integer]
    simplices0 n = [0..(n-1)]

    linkofedge2D :: [Integer] -> [[Integer]] -> [[Integer]]
    linkofedge2D [] faces = faces
    linkofedge2D e:dge faces = filter e (linkofedge2D dge faces)
