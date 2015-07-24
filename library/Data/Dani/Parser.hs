module Data.Dani.Parser (
        module Data.Dani.Types
    ) where

import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import Data.Char

import Data.Dani.Types

symbolParser :: P.Parser Symbol  
symbolParser = symbol <$> P.takeWhile1 isAlpha  

stringParser :: P.Parser T.Text
stringParser = P.char '"' *> P.takeWhile1 (\c -> not (c=='"'))  <* P.char '"' 

skipSpaceAndComma :: P.Parser ()
skipSpaceAndComma = P.skipWhile (\c -> isSpace c || c==',')

elementParser :: P.Parser Dn
elementParser = undefined

listParser :: P.Parser [Dn] 
listParser = P.char '(' *> undefined 

