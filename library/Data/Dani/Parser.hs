module Data.Dani.Parser (
        module Data.Dani.Types
    ) where

import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import Data.Char
import Control.Applicative
import Control.Comonad.Env
import Control.Comonad.Trans.Cofree

import Data.Dani.Types

parser :: P.Parser Dn
parser = P.skipSpace *> elementParser

elementParser :: P.Parser Dn
elementParser = liftA (CofreeT . env [] . (:<) ()) $ 
    liftA Symbol symbolParser <|> 
    liftA String stringParser <|> 
    liftA Number P.scientific <|> 
    liftA List listParser

symbolParser :: P.Parser Symbol
symbolParser = symbol <$> P.takeWhile1 isAlpha  

stringParser :: P.Parser T.Text
stringParser = 
    P.char '"' *> P.takeWhile1 (\c -> not (c=='"'))  <* P.char '"' 

skipSpaceAndComma :: P.Parser ()
skipSpaceAndComma = P.skipWhile (\c -> isSpace c || c==',')

listParser :: P.Parser [Dn]
listParser = P.char '(' *> P.skipSpace *> P.manyTill elementParser' (P.char ')')
  where
    elementParser' = elementParser <* skipSpaceAndComma

