{-# OPTIONS -W #-}
{-# LANGUAGE OverloadedStrings, TupleSections #-}

module NounPhrase (main) where

import Prelude hiding (lookup)
import Prob
import Bless
import Control.Monad
import Grammar hiding (main)
import Observed hiding (main)
import Data.Ord
import Data.List hiding (lookup, insert)
import Data.Map hiding (map, filter, null, foldr, foldl, foldr', foldl')
import Util (putStrsLnToFile, buffered)
import qualified Data.Map as M

(//) :: a -> (a -> b) -> b
x // f = f x

topbot :: (a -> String) -> [a] -> IO ()
topbot f xs = case splitAt (n + n) xs of
              (_ ,[]) -> display xs
              (ys,zs) -> do display (take n ys)
                            putStrLn "..."
                            display (reverse (take n (reverse zs)))
  where display = mapM_ (putStrLn . f)
        n = 10

distdiff :: (Show a, Ord a) => Dist a -> Dist a -> IO ()
distdiff a b = printDist (unionWith (+) a (M.map negate b))

printDist :: (Show a) => Dist a -> IO ()
printDist = topbot showEntry . sortBy (comparing snd) . toList

showEntry :: (Show a) => (a, Prob) -> String
showEntry (x,p) = show (fromRational p :: Float) ++ " " ++ show x

-- The 28th-30th most frequent head words with POS NN are sale, issue, and profit
in1 = top_w_counts ! "NN" // fst // toList // sortBy (comparing snd) // length
-- 3260
in2 = top_w_counts ! "NN" // fst // toList // sortBy (comparing snd) // drop 3230 // take 3
-- [(profit/,1180),(issue/,1182),(sale/,1185)]

-- NP(NNS?) parent tends to go to NPB head child
-- NPB(NNS?) parent tends to go to NNS? head child
--  > head_counts ! "NP" // snd ! "NN" // fst
--  fromList [(NN,93),(NP,3285),(NX,5),(NPB,97965)]
--  > head_counts ! "NP" // snd ! "NNS" // fst
--  fromList [(NP,2204),(NX,6),(NPB,55328),(NNS,35)]
--  > head_counts ! "NPB" // snd ! "NN" // fst
--  fromList [(NN,97691),(QP,118),(NX,145),(RRC,1),(ADJP,9),(SBAR,1)]
--  > head_counts ! "NPB" // snd ! "NNS" // fst
--  fromList [(NP,1),(QP,9),(NX,63),(NNS,55253),(ADJP,1),(SBAR,1)]
-- So let's take a look at what modifiers tend to accompany these transitions

-- In general, NN^NPB has more modifiers on the left, whereas NPB^NP has more modifiers on the right, as a linguist would expect
--  > [ (side, npb_nt_counts ! (side, "NN") // snd ! "NN" // fst // elems // sum) | side <- [L,R] ]
--  [(L,129044),(R,97702)]
--  > [ (side, npb_nt_counts ! (side, "NNS") // snd ! "NNS" // fst // elems // sum) | side <- [L,R] ]
--  [(L,58786),(R,55254)]
--  > [ (side, mod_nt_counts ! (side, "NP", "NPB") // snd ! "NN" // fst // elems // sum) | side <- [L,R] ]
--  [(L,98007),(R,139791)]
--  > [ (side, mod_nt_counts ! (side, "NP", "NPB") // snd ! "NNS" // fst // elems // sum) | side <- [L,R] ]
--  [(L,55338),(R,74952)]

r_np_npb           = np_npb R
r_np_npb_nn_nt_w   = r_np_npb ! "NN"
r_np_npb_nns_nt_w  = r_np_npb ! "NNS"
l_npb              = npb L
l_npb_nn_nn_nt_w   = l_npb ! "NN"
l_npb_nns_nns_nt_w = l_npb ! "NNS"

fromListFunction :: (Ord a) => [a] -> (a -> b) -> Map a b
fromListFunction xs f = fromList [ (x, f x) | x <- xs ]

np_npb, npb :: Side -> Map POS -- the valid POSs are NN and NNS
               (Dist (Maybe (Label, Word)), -- backoff ("average") distribution
       Map Lex (Dist (Maybe (Label, Word)))) -- Nothing means +STOP+

np_npb side{-probably R-} = fromListFunction nominals $ \posHead ->
  let nt_dist = mod_nt_dists ! (side, "NP", "NPB") // snd ! posHead
      go' = go (seek "mod_w_dists" mod_w_dists)
               (\mod -> lookup (mod, side, "NP", "NPB", posHead))
  in (go' Nothing (fst nt_dist), mapWithKey (go' . Just) (snd nt_dist))

npb side{-probably L-} = fromListFunction nominals $ \posHead ->
  let nt_dist = npb_nt_dists ! (side, posHead) // snd ! posHead
      go' = go (seek "npb_w_dists" npb_w_dists)
               (\mod -> lookup (mod, side, posHead, posHead))
  in (go' Nothing (fst nt_dist), mapWithKey (go' . Just) (snd nt_dist))

go :: (Ord history) =>
      (POS ->             (Dist Lex,
              Map history ((Prob, Dist Lex),
              Map Lex     ((Prob, Dist Lex))))) ->
      (Label -> Map history ((Prob, Dist Lex),
                Map Lex     ((Prob, Dist Lex))) ->
                Maybe         ((Prob, Dist Lex),
                      Map Lex ((Prob, Dist Lex)))) ->
      Maybe Lex -> Dist (Label, POS) -> Dist (Maybe (Label, Word))
go level2_function level1_function lexHead m = fromList $ do
  ((mod, pos), wt1) <- toList m
  if mod == _STOP_ then return (Nothing, wt1) else do
  (lex, wt2) <- toList $
    let (level2, m2) = level2_function pos in
    case level1_function mod m2 of
    Nothing -> level2
    Just ((deficiency1, dist1), m1) ->
      let level1 = unionWith (+) (M.map (deficiency1 *) level2) dist1 in
      case lexHead >>= (`lookup` m1) of
      Nothing -> level1
      Just (deficiency0, dist0) ->
        unionWith (+) (M.map (deficiency0 *) level1) dist0
  return (Just (mod, Word lex pos), wt1 * wt2)

main = do
  done <- fmap (fromList . map ((,()) . parse_bless_test) . lines)
               (readFile "NounPhrase.out.1")
  tests <- blessTests (\w@(Word _ pos) -> pos `elem` nominals && known w)
  putStrsLnToFile "NounPhrase.out"
    $ buffered
    $ [ intercalate "\t"
        [ show $ concept
        , show $ relation
        , show $ relatum
        , show $ liftM2 kl'kl r_np_npb_of_concept r_np_npb_of_relatum
        , show $ liftM2 kl'kl r_np_npb_of_relatum r_np_npb_of_concept
        , show $ liftM2 kl l_npb_of_concept l_npb_of_relatum
        , show $ liftM2 kl l_npb_of_relatum l_npb_of_concept ]
      | (concept, relation, relatum) <- filter (`notMember` done) tests
      , let r_np_npb_of_concept = r_np_npb_of_word concept
            r_np_npb_of_relatum = r_np_npb_of_word relatum
            l_npb_of_concept    = l_npb_of_word    concept
            l_npb_of_relatum    = l_npb_of_word    relatum ]

r_np_npb_of_word, l_npb_of_word :: Word -> Maybe (Dist (Maybe (Label, Word)))
r_np_npb_of_word (Word lex pos) = r_np_npb ! pos // snd // lookup lex
l_npb_of_word    (Word lex pos) = l_npb    ! pos // snd // lookup lex

parse_bless_test :: String -> (Word, Relation, Word)
parse_bless_test s = (read conceptStr, read relationStr, read relatumStr)
  where conceptStr:relationStr:relatumStr:_ = words s

nominals :: [POS]
nominals = expandPOS 'n'
