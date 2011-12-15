module Merge (merge, mergeBy) where

import PriorityQueue.PriorityQueue

merge :: (Ord a) => [[a]] -> [a]
merge = mergeBy compare

mergeBy :: (a -> a -> Ordering) -> [[a]] -> [a]
mergeBy cmp xss = go (fromListBy cmp' [ (x,xs) | x:xs <- xss ])
  where cmp' (x,_) (y,_) = cmp x y
        go q | isEmpty q = []
             | otherwise = let q' = deleteMinBy cmp' q in seq q'
                           (case findMin q of {(x,xs) ->
                            (x : go (case xs of
                                     [] -> q'
                                     x:xs -> insertBy cmp' (x,xs) q'))})

