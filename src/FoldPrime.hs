-----------------------------------------------------------------------------
--
-- Module      :  FoldPrime
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

module FoldPrime (
    foldl'
) where
    foldl' f z []     = z
    foldl' f z (x:xs) = let z' = z `f` x
                   in seq z' $ foldl' f z' xs

