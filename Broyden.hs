{-# OPTIONS -W #-}
module Broyden (broyden, broyden', zero) where

import Data.List
import Data.Array.IArray

broyden, broyden' :: (IArray a e, Ix i, RealFloat e) =>
                     (a i e -> a i e) -> Int -> a i e -> (Int, a i e)

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
          or (zipWith (\x1 s -> x1/=0 && abs (s/x1) >= 1e-10) (elems x1) (elems s))
        path = zip (zip [0..] x) (zip3 (tail x) s ss)
        x = init : zipWith plus x s
        z = [ foldl' (\z (sj:sj1:_, sjsj) ->
                      z `plus` amap ((sj `dot` z / sjsj) *) sj1)
                     zinit
                     (zip (take k (tails s)) ss)
            | (k, zinit) <- zip [0..] (map f (tail x)) ]
        s = f init : zipWith3 (\z sk sksk -> amap (/ (1 - sk `dot` z / sksk)) z)
                              z s ss
        ss = [ sj `dot` sj | sj <- s ]
        plus u v = listArray (bounds init) (zipWith (+) (elems u) (elems v))
        dot  u v = sum (zipWith (*) (elems u) (elems v))

zero :: (IArray a e, Ix i, Num e) => (i, i) -> a i e
zero bounds = accumArray undefined 0 bounds []
