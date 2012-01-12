{-# OPTIONS -W #-}

module Chart (
    Chart(..), Cell(..),
    infixChart, prefixChart, suffixChart, exactChart
) where

import Graph (Vertex)
import Data.Word (Word8)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)

data Chart = Chart {
  minCell     :: Cell,
  maxCell     :: Cell,
  levels      :: [[Cell]],
  indexOfCell :: Cell -> Int,
  cellOfIndex :: Int -> Cell,
  splits      :: !(Cell -> [(Cell, Cell)]),
  epsilon     :: !(Cell -> Double),
  terminal    :: !(Cell -> Vertex -> Double)
}

data Cell = Universe
          | Epsilon
          | Range {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
            -- 0 <= i < j <= n
  deriving (Eq, Ord, Show)

infixChart :: [Vertex] -> Chart
infixChart []        = universeChart
infixChart terminals = mixfixChart True True terminals

prefixChart :: [Vertex] -> Chart
prefixChart []        = universeChart
prefixChart terminals = mixfixChart False True terminals

suffixChart :: [Vertex] -> Chart
suffixChart []        = universeChart
suffixChart terminals = mixfixChart True False terminals

exactChart :: [Vertex] -> Chart
exactChart = mixfixChart False False

universeChart :: Chart
universeChart = Chart {
  minCell = Universe,
  maxCell = Universe,
  levels  = [[Universe]],
  indexOfCell = \Universe -> 0,
  cellOfIndex = \0 -> Universe,
  splits = \Universe -> [(Universe, Universe)],
  epsilon = \Universe -> 1,
  terminal = \Universe _t -> 1
}

mixfixChart :: Bool -> Bool -> [Vertex] -> Chart
mixfixChart before after terminals = Chart {
  minCell     = cells ! fst (bounds cells),
  maxCell     = cells ! snd (bounds cells),
  levels      = levels,
  indexOfCell = \c -> case c of
                      Universe -> 0
                      Epsilon -> 1
                      Range i j -> 2 + (2 + 2 * fi n + fi i - fi j)
                                       * (fi j - fi i - 1) `div` 2
                                     + fi i,
  cellOfIndex = \i -> cells ! i,
  splits      = \c -> case c of
                      Universe -> [(Universe, Universe)]
                      Epsilon -> [(Epsilon, Epsilon)]
                      Range i j -> let stay k | before && k == 0
                                              || after && k == n = Universe
                                              | otherwise        = Epsilon
                                   in [(stay i, c)]
                                   ++ [(Range i k, Range k j) | k <- [i+1..j-1]]
                                   ++ [(c, stay j)],
  epsilon     = \c -> case c of
                      Universe -> 1
                      Epsilon -> 1
                      Range _ _ -> 0,
  terminal    = \c -> case c of
                      Universe -> \_t -> 1
                      Epsilon -> \_t -> 0
                      Range i j | i+1 == j -> let t' = a ! j in t' `seq`
                                              \t -> if t == t' then 1 else 0
                                | otherwise -> \_t -> 0
} where n :: Word8
        n = fromIntegral (length terminals)
        a :: UArray Word8 Vertex
        a = listArray (1,n) terminals
        fi :: Word8 -> Int
        fi = fromIntegral
        cells :: Array Int Cell
        cells = listArray (if before || after then 0 else 1,
                           fi n * (fi n + 1) `div` 2 + 1)
                          (concat levels)
        levels :: [[Cell]]
        levels = ([Universe | before || after] ++ [Epsilon])
               : [ [Range i (i+l) | i <- [0..n-l]] | l <- [1..n] ]
