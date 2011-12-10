{-# OPTIONS -W #-}

module Reduce (reduce, mapReduce) where

import Data.Monoid
import Data.Maybe

-- MapReduce over a long list

reduce :: Monoid a => [a] -> a
reduce []     = mempty
reduce [x]    = x
reduce (x:xs) = reduce (scan xs [Just x]) where
  scan []     accum = catMaybes (reverse accum)
  scan (x:xs) accum = push x xs accum 0
  push x _  _             n | x `seq` n `seq` False = undefined
  push x xs []            n = x : scan xs (replicate (succ n) Nothing)
  push x xs (Nothing: ys) n =     scan xs (replicate n Nothing ++ Just x : ys)
  push x xs (Just y : ys) n = push (mappend y x) xs ys (succ n)

mapReduce :: Monoid a => (b -> a) -> [b] -> a
mapReduce f = reduce . map f

-- Testing

data Tree = Leaf Int | Branch Tree Tree deriving (Eq, Ord, Show)
instance Monoid Tree where
  mempty  = Leaf 0
  mappend = Branch

