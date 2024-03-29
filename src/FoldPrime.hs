-----------------------------------------------------------------------------
--
-- Module      :  FoldPrime
--
-- The standard fold' improvement on fold
--
-----------------------------------------------------------------------------

module FoldPrime (
    foldl'
) where
    foldl' f z []     = z
    foldl' f z (x:xs) = let z' = z `f` x
                   in seq z' $ foldl' f z' xs

