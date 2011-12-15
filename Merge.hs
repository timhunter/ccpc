module Merge (merge, mergeBy, crossWith, crossBy, crossByWith) where

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

crossWith :: (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
crossWith = crossByWith compare

crossBy :: ((a,b) -> (a,b) -> Ordering) -> [a] -> [b] -> [(a,b)]
crossBy cmp = crossByWith cmp (,)

crossByWith :: (c -> c -> Ordering) -> (a -> b -> c) -> [a] -> [b] -> [c]
crossByWith cmp f xs ys = mergeBy cmp [ map (f x) ys | x <- xs ]
