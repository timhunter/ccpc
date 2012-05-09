{-# OPTIONS -W #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module Prob (
    Count, Counts, Prob, Dist,
    rough, smoothedBy, smoothBy, smoothed, smooth,
    Obs(Obs, past, future, count), Tally(tally), Tallied,
    kl, kl'kl, js
) where

import Data.Ratio ((%))
import Data.Either (partitionEithers)
import qualified Data.Map as M

type Count    = Int
type Counts v = M.Map v Count
type Prob     = Rational
type Dist   v = M.Map v Prob

rough :: Counts a -> Dist a
rough counts = M.map ((% total) . fromIntegral) counts
  where total = fromIntegral (sum (M.elems counts))

smoothedBy :: (Ord a) => (Count -> Count) -> Dist a -> Counts a -> Dist a
smoothedBy backoffOfDiversity coarser finer
  = M.unionWith (+) (M.map (deficiency *) coarser) rescaled
  where (deficiency, rescaled) = smoothBy backoffOfDiversity finer

smoothBy :: (Ord a) => (Count -> Count) -> Counts a -> (Prob, Dist a)
smoothBy backoffOfDiversity finer
  -- Bikel equation (19):
  -- Setting backoffOfDiversity to (5 *)     gives Bikel's f_t = 0 and f_f = 5.0
  -- Setting backoffOfDiversity to (const 5) gives Bikel's f_t = 5.0 and f_f = 0
  = (fromIntegral backoff % denominator,
     M.map ((% denominator) . fromIntegral) finer)
  where total       = sum (M.elems finer)
        diversity   = M.size finer
        backoff     = backoffOfDiversity diversity
        denominator = fromIntegral (total + backoff)

smoothed :: (Ord a) => Dist a -> Counts a -> Dist a
smooth   :: (Ord a) => Counts a -> (Prob, Dist a)
smoothed = smoothedBy (5 *)
smooth   = smoothBy   (5 *)

data Obs k v = Obs { past :: k, future :: v, count :: Count }

class Tally k v where
  type Tallied k v
  tally :: (Ord v) => [Obs k v] -> (Counts v, Tallied k v)

instance Tally () v where
  type Tallied () v = ()
  tally os = (M.fromListWith (+) [ (v, c) | Obs () v c <- os ], ())

instance (Ord k1, Tally k2 v) => Tally (k1, k2) v where
  type Tallied (k1, k2) v = M.Map k1 (Counts v, Tallied k2 v)
  tally os = (M.unionsWith (+) (map fst (M.elems m)), m)
    where m = M.map tally (M.fromListWith (++)
                [ (k1, [Obs k2 v c]) | Obs (k1, k2) v c <- os ])

instance (Tally k v) => Tally (Maybe k) v where
  type Tallied (Maybe k) v = Tallied k v
  tally os = (M.unionWith (+) cNothing cJust, rJust)
    where (cNothing, ()) = tally osNothing
          (cJust, rJust) = tally osJust
          (osNothing, osJust) = partitionEithers (map f os)
          f (Obs Nothing  v c) = Left  (Obs () v c)
          f (Obs (Just k) v c) = Right (Obs k  v c)

-- Compute the KL divergence between two distributions,
-- or return a list of entries to explain why the KL divergence is infinite
kl :: (Ord a) => Dist a -> Dist a -> Either [(a,Prob)] Double
kl a b = M.foldl' g (Right 0) $
         M.differenceWith f (M.mapWithKey (\a p -> Left (a, p)) a) b
  where f (Left (_,p)) q | p >= 0 && q > 0
                 = Just (Right (fromRational p * log (fromRational (p/q))))
        f orig _ = Just orig
        g (Left xs) (Left x)  = Left (xs++[x])
        g (Right _) (Left x)  = Left [x]
        g (Left xs) (Right _) = Left xs
        g (Right y) (Right x) = Right (x+y)

-- Compute the KL divergence for generating a sequence of values by drawing
-- repeatedly from a distribution of Maybe-values until Nothing is chosen.
kl'kl :: (Ord a) => Dist (Maybe a) -> Dist (Maybe a) ->
                    Either [(Maybe a,Prob)] (Double, Double)
kl'kl a b = fmap (\computed -> (computed / pStop, computed)) (kl a b)
  where pStop = maybe 0 fromRational (M.lookup Nothing a)

-- Compute the Jensen-Shannon divergence, which is never infinite
js :: (Ord a) => Dist a -> Dist a -> Double
js a b = (kl_a + kl_b) / 2
  where Right kl_a = kl a avg
        Right kl_b = kl b avg
        avg = M.unionWith (+) (half a) (half b)
        half = M.map (/2)
