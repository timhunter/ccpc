{-# OPTIONS -W #-}

module Chart where

import Graph (Vertex)
import Data.Word (Word8)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)

data Chart = Chart {
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
infixChart terminals = Chart {
  levels = levels,
  indexOfCell = \c -> case c of
                      Universe -> 0
                      Epsilon -> 1
                      Range i j -> 2 + (2 + 2 * fi n + fi i - fi j)
                                       * (fi j - fi i - 1) `div` 2
                                     + fi i,
  cellOfIndex = \i -> cells ! fromIntegral i,
  splits      = \c -> case c of
                      Universe -> [(Universe, Universe)]
                      Epsilon -> [(Epsilon, Epsilon)]
                      Range i j -> let stay k | k == 0 || k == n = Universe
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
        size :: Int
        size = fi n * (fi n + 1) `div` 2 + 2
        fi :: Word8 -> Int
        fi = fromIntegral
        cells :: Array Int Cell
        cells = listArray (0,size-1) (concat levels)
        levels :: [[Cell]]
        levels = [Universe, Epsilon]
               : [ [Range i (i+l) | i <- [0..n-l]] | l <- [1..n] ]
