{-# OPTIONS -W #-}

module Reduce (reduce, mapReduce) where

import Data.Monoid
import Data.Maybe (catMaybes)
import Data.List (foldl')

-- MapReduce over a long list

reduce :: Monoid a => [a] -> a
reduce []     = mempty
reduce [x]    = x
reduce (x:xs) = reduce (reverse (catMaybes (foldl' (shift 0) [Just x] xs)))
  where shift n _             x | n `seq` x `seq` False = undefined
        shift n []            x = replicate n Nothing ++ Just x : []
        shift n (Nothing: ys) x = replicate n Nothing ++ Just x : ys
        shift n (Just y : ys) x = shift (succ n) ys (mappend y x)

mapReduce :: Monoid a => (b -> a) -> [b] -> a
mapReduce f = reduce . map f

-- Testing

data Tree = Leaf Int | Branch Tree Tree deriving (Eq, Ord, Show)
instance Monoid Tree where
  mempty  = Leaf 0
  mappend = Branch

