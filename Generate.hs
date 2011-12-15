{-# OPTIONS -W #-}

module Generate (main) where

import Data.List (intercalate)
import Control.Monad (liftM, liftM2, forever)
import Control.Monad.State.Lazy (MonadState, get, put, runState)
import CFG (CFG, Vertex, isTerminal)
import Data.Binary (decodeFile)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Util (decodeListFromFile)
import Merge (mergeBy, crossByWith)
import qualified System.Random as R
import qualified Data.ByteString.Lazy.Char8 as B

data Item = LB | RB | Fail | Lex Vertex deriving (Show)
type Phrase = [Item]
type Stream = [(Double, Maybe Phrase)]

main :: IO ()
main = do
  theCFG <- decodeFile "wsj.cfg" :: IO CFG
  -- thePartition <- decodeListFromFile "wsj.car-dealer.partition" :: IO [UArray Vertex Double]
  theWordsFile <- B.readFile "wsj.words" :: IO B.ByteString
  let theWords :: Array Vertex B.ByteString
      theWords = let bs = B.lines theWordsFile
                 in listArray (1, length bs) bs
      show' :: Item -> String
      show' LB      = "["
      show' RB      = "]"
      show' Fail    = "?"
      show' (Lex n) = B.unpack (theWords ! (-n))
      showPhrase = intercalate " " . map show'
  mapM_ (\(wt, phrase) -> do
          putStr (show wt)
          putStrLn (case phrase of Nothing -> ""
                                   Just phrase -> " " ++ showPhrase phrase))
        (top theCFG 0)
  {-
  forever $ R.getStdRandom (runState (generate theCFG (head thePartition) 0))
            >>= putStrLn . showPhrase
  -}

random :: (R.RandomGen g, MonadState g m, R.Random a) => m a
random = get >>= \g -> let (a,g') = R.random g in put g' >> return a

randomR :: (R.RandomGen g, MonadState g m, R.Random a) => (a,a) -> m a
randomR b = get >>= \g -> let (a,g') = R.randomR b g in put g' >> return a

-- Try: take 5 $ filter (Maybe.isJust.snd) $ top aCFG 0

top :: CFG -> Vertex -> Stream
top cfg = f where
  f lhs
    | isTerminal lhs = [(1, Just [Lex lhs])]
    | otherwise = (1, Nothing) : mergeBy cmp
        [ map (scale wt)
              (case rhs of [] -> identity
                           _  -> foldr1 cross (map f rhs))
        | (wt, rhs) <- cfg ! lhs ]
    where cross = crossByWith cmp multiply
          scale p (q,ys) = (p*q, ys)
          multiply (p,xs) (q,ys) = (p*q, liftM2 (++) xs ys)
          identity = [(1, Just [])]
          cmp (p,_) (q,_) = compare q p

aCFG :: CFG
aCFG = listArray (0,0) [[(1/3, [-1]), (1/3, [0])]]

generate :: (R.RandomGen g, MonadState g m) =>
            CFG -> UArray Vertex Double -> Vertex -> m Phrase
generate cfg prt = f 100 where
  f fuel lhs
    | fuel < 0       = return [Fail]
    | isTerminal lhs = return [Lex lhs]
    | otherwise      = randomR (0, prt ! lhs)
                       >>= liftM concat'
                         . mapM (f (pred fuel))
                         . choose [ (wt * product (map part rhs), rhs)
                                  | (wt, rhs) <- cfg ! lhs ]
  part cat | isTerminal cat = 1
           | otherwise      = prt ! cat
  concat' xs | length xs > 2 = [LB] ++ concat xs ++ [RB]
             | otherwise     =         concat xs

choose :: [(Double, a)] -> Double -> a
choose []              r = error ("choose [] " ++ show r)
choose ((wt,rhs):rhss) r = let r' = r - wt in
                           if r' < 0 || null rhss then rhs else choose rhss r'
