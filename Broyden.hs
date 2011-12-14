{-# OPTIONS -W #-}
module Broyden (broyden, broyden', zero, Doubles) where

import Data.List
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Fusion.Stream as Stream

type Doubles = S.Vector Double

broyden, broyden' :: (Doubles -> Doubles) -> Int -> Doubles -> (Int, Doubles)

broyden f = loop 0
  -- outer loop with restarts every 20 iterations
  -- (Nederhof & Satta, bottom of page 150)
  where loop k steps init = if steps <= 0 then (k, init) else
                            let (j, x) = broyden' f (min 20 steps) init in
                            if j < 20 then (k + j, x) else
                            loop (k + j) (steps - j) x

broyden' f steps init = fst (head (dropWhile continue path))
  where continue ((k,_x),(x1,s,ss)) =
          k < steps && ss > 0 && not (isDenormalized ss) &&
          Stream.or (Stream.zipWith (\x1 s -> x1/=0 && abs (s/x1) >= 1e-10) (G.stream x1) (G.stream s))
        path = zip (zip [0..] x) (zip3 (tail x) s ss)
        x, z, s :: [Doubles]
        x = init : zipWith plus x s
        z = [ foldl' (\z (sj:sj1:_, sjsj) ->
                      G.zipWith ((+) . ((sj `dot` z / sjsj) *)) sj1 z)
                     zinit
                     (zip (take k (tails s)) ss)
            | (k, zinit) <- zip [0..] (map f (tail x)) ]
        s = f init : zipWith3 (\z sk sksk -> G.map (/ (1 - sk `dot` z / sksk)) z)
                              z s ss
        ss :: [Double]
        ss = [ sj `dot` sj | sj <- s ]
        plus :: Doubles -> Doubles -> Doubles
        plus = G.zipWith (+)
        dot :: Doubles -> Doubles -> Double
        dot u v = Stream.foldl' (+) 0 (Stream.zipWith (*) (G.stream u) (G.stream v))

zero :: Int -> Doubles
zero ln = S.replicate ln 0
