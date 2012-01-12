{-# OPTIONS -W #-}

module Reduce (reduce, reduceBy, mapReduce, mapReduceBy) where

import Data.Monoid
import Data.List (foldl')
import Control.Parallel (par)

-- MapReduce over a long list

reduce :: Monoid a => [a] -> a
reduce = reduceBy mempty mappend

reduceBy :: a -> (a -> a -> a) -> [a] -> a
reduceBy mempty mappend = reduce where
  reduce []       = mempty
  reduce [x]      = x
  reduce (x:y:xs) = reduce (reverse (map snd
                      (foldl' (shiftBy mappend 0) [(1, mappend x y)] xs)))

shiftBy :: (a -> a -> a) -> Int -> [(Int,a)] -> a -> [(Int,a)]
shiftBy _mappend n _          x | (case compare n 9 of 
                                   LT -> id
                                   EQ -> par x
                                   GT -> if n >= 12 then seq x else id)
                                  False = undefined
shiftBy _mappend n []         x = [(n,x)]
shiftBy  mappend n ((0,y):ys) x = shiftBy mappend (succ n) ys (mappend y x)
shiftBy _mappend n ((m,y):ys) x = (n,x):(pred m,y):ys

mapReduce :: Monoid a => (b -> a) -> [b] -> a
mapReduce f = reduce . map f

mapReduceBy :: (b -> a) -> a -> (a -> a -> a) -> [b] -> a
mapReduceBy f mempty mappend = reduceBy mempty mappend . map f

-- Testing

data Tree = Leaf Int | Branch Tree Tree deriving (Eq, Ord, Show)
instance Monoid Tree where
  mempty  = Leaf 0
  mappend = Branch

