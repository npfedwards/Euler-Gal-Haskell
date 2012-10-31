-----------------------------------------------------------------------------
--
-- Module      :  EGPowerSeries
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

module EGPowerSeries (
    pullChi,
    egStarGraph
) where
    import Polynomials
    import Data.List

    pullChi :: [Integer] -> Integer -> Integer
    pullChi list chi = genericIndex list chi

    egStarGraph :: Integer -> Integer -> [Integer]
    egStarGraph edges robots = multiplyBrackets (taylorExpand 1 edges robots) [1, 1-edges]




