{-# OPTIONS -W #-}

module BagOfWords (main) where

import Bless (blessTests, expandPOS)
import Prob (Counts, Dist, smoothed, rough, kl)
import Observed (Sexp(..), theTrees, Word(..), Lex(..), Label(..), POS, _UNKNOWN_)
import Util (putStrsLnToFile, MonoidMap(MonoidMap), buffered)
import Reduce (mapReduce, reduceBy)
import Data.List (intercalate)
import Data.Monoid (Monoid(..), Sum(..))
import Control.Exception (evaluate)
import Control.DeepSeq (NFData(rnf))
import qualified Data.Map as M

wordsInSexp :: Sexp -> [Word]
wordsInSexp (List [Atom pos, Atom lex]) = [Word (Lex lex) (Label pos)]
wordsInSexp (List (Atom _ : es)) = concatMap wordsInSexp es
wordsInSexp e = error ("Unexpected sexp " ++ show e)

-- Only look at nouns, verbs, and adjectives, and ignore POS distinctions
-- within each of these three categories (e.g., map NNS to NN)
contentPOSs :: M.Map POS POS
contentPOSs = M.fromList [ (pos, pos0)
                         | p <- ['n', 'v', 'j']
                         , let poss@(pos0:_) = expandPOS p
                         , pos <- poss ]

contentWord :: Word -> Maybe Word
contentWord (Word lex pos) = fmap (Word lex) (M.lookup pos contentPOSs)

unigrams :: M.Map Word Int
unigrams = M.mapMaybe threshold m
  where MonoidMap m = mapReduce f theTrees
        f sent = MonoidMap m
          where m = M.fromListWith mappend
                     [ (w, Sum 1)
                     | Just w <- map contentWord (wordsInSexp sent) ]
        threshold (Sum count) = if count > 5 then Just count else Nothing

known :: Word -> Word
known w@(Word _lex pos) | M.member w unigrams = w
                        | otherwise = Word _UNKNOWN_ pos

bigrams :: [Sexp] -> M.Map Word (Counts Word)
bigrams trees = M.map (\(MonoidMap m') -> M.map getSum m') m
  where MonoidMap m = mapReduce f trees
        f sent = MonoidMap (M.map (const (MonoidMap m)) m)
          where m = M.fromListWith mappend
                     [ (known w, Sum 1)
                     | Just w <- map contentWord (wordsInSexp sent) ]

dists :: M.Map Word (Counts Word) -> Word -> Dist Word
dists table = \w -> maybe backoff (smoothed backoff) (M.lookup w table)
  where backoff = rough (reduceBy M.empty (M.unionWith (+)) (M.elems table))

-- TODO: optimize smoothing in dists by maximizing llike on theTrees'
llike :: (Word -> Dist Word) -> M.Map Word (Counts Word) -> Double
llike pr obs = sum [ sum [ fromIntegral n * log (fromRational (prw M.! c))
                         | (c,n) <- M.toList cc ]
                   | (w,cc) <- M.toList obs
                   , let prw = pr w ]

main = do
  evaluate (rnf unigrams)
  let sem = dists (bigrams theTrees)
  tests <- blessTests (\w@(Word _ pos) ->
            M.lookup pos contentPOSs == Just pos &&
            M.member w unigrams)
  putStrsLnToFile "BagOfWords.data"
    $ buffered
    $ map (intercalate "\t")
    $ words "Concept Relation Relatum BagOfWords_KL BagOfWords_LK"
    : [ [ show concept
        , show relation
        , show relatum
        , either (const "NA") show (kl sem_of_concept sem_of_relatum)
        , either (const "NA") show (kl sem_of_relatum sem_of_concept) ]
      | (concept, relation, relatum) <- tests
      , let sem_of_concept = sem concept
            sem_of_relatum = sem relatum ]
