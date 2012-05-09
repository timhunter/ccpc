module Bless (Relation(..), blessTests, expandPOS) where

import Data.Char (isSpace, isAlpha, toLower)
import Observed (Word(Word), Lex(Lex), Label(Label), POS, thePOSs)
import Data.Atom.Simple (intern)

data Relation = Attri | Coord | Event | Hyper | Mero
              | RandomJ | RandomN | RandomV
  deriving (Eq, Ord, Show)

instance Read Relation where
  readsPrec _ = readParen False (\r ->
    let (word, rest) = break isSpace (dropWhile isSpace r) in
    case map toLower (filter isAlpha word) of
    "attri"   -> [(Attri  , rest)]
    "coord"   -> [(Coord  , rest)]
    "event"   -> [(Event  , rest)]
    "hyper"   -> [(Hyper  , rest)]
    "mero"    -> [(Mero   , rest)]
    "randomj" -> [(RandomJ, rest)]
    "randomn" -> [(RandomN, rest)]
    "randomv" -> [(RandomV, rest)]
    _         -> [])

blessTests :: (Word -> Bool) -> IO [(Word, Relation, Word)]
blessTests vocab = do
  blessFile <- readFile "BLESS.txt"
  return [ (concept, read relationStr, relatum)
         | line <- lines blessFile
         , let [conceptStr, _, relationStr, relatumStr] = words line
         , let relata = expand relatumStr
         , not (null relata)
         , concept <- expand conceptStr
         , relatum <- relata ]
  where
    expand :: String -> [Word]
    expand cs =
      case splitAt (length cs - 2) cs of
      (lex, ['-',p]) -> filter vocab
                          (map (Word (Lex (intern lex))) (expandPOS p))
      _ -> []

expandPOS :: Char -> [POS]
expandPOS 'n' = map (Label . intern) (words "NN NNS")
expandPOS 'v' = map (Label . intern) (words "VB VBD VBG VBN VBP VBZ")
expandPOS 'j' = map (Label . intern) (words "JJ JJR JJS")
expandPOS _   = []
