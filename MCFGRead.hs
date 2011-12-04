module MCFGRead (mcfgFromFile) where

import MCFG
import Data.Function (on)
import Data.Maybe (catMaybes)
import Control.Monad (liftM, liftM2)
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec
import Text.Parsec.ByteString (Parser, parseFromFile)
import qualified Text.Parsec.Token as P

P.TokenParser {
    P.identifier     = identifier,
    P.symbol         = symbol,
    P.decimal        = decimal,
    P.naturalOrFloat = naturalOrFloat,
    P.brackets       = brackets,
    P.semiSep1       = semiSep1,
    P.comma          = comma,
    P.whiteSpace     = whiteSpace
} = P.makeTokenParser P.LanguageDef {
    P.commentStart    = "(*",
    P.commentEnd      = "*)",
    P.commentLine     = "#",
    P.nestedComments  = False,
    P.identStart      = letter   <|> oneOf "_-",
    P.identLetter     = alphaNum <|> oneOf "_-",
    P.opStart         = parserZero,
    P.opLetter        = parserZero,
    P.reservedNames   = [],
    P.reservedOpNames = [],
    P.caseSensitive   = True
}

quote :: Parser String
quote = symbol "\""

around :: Parser b -> (a -> c -> d) -> Parser a -> Parser c -> Parser d
around delim f p q = do x <- p
                        delim
                        y <- q
                        return (f x y)

mcfgFromFile :: String -> MCFG
mcfgFromFile name = unsafePerformIO (do
    result <- parseFromFile (between whiteSpace eof mcfg) name
    either (fail . show) return result)

mcfg :: Parser MCFG
mcfg = many (liftM2 (,) (optionMaybe weight) rule)

weight :: Parser Rational
weight = chainl1 (liftM (either toRational toRational) naturalOrFloat)
                 (symbol "/" >> return (/))

rule :: Parser (Cat, RHS Cat)
rule = around (symbol "-->") (,) identifier rhs

rhs :: Parser (RHS Cat)
rhs = liftM2 Cats (many1 identifier)
        (many1 (fmap catMaybes (brackets (semiSep1 component))))
  <|> between quote quote
        (option (Cats [] [[]]) (liftM Term identifier))

component :: Parser (Maybe (Int, Int))
component = (symbol "Epsilon" >> return Nothing)
        <|> (liftM Just (around comma ((,) `on` fromIntegral) decimal decimal))

