module Data.Dani.Parser (
        module Data.Dani
    ) where

import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import Data.Char
import Data.Functor.Identity
import Control.Applicative
import Control.Comonad.Env
import Control.Comonad.Trans.Cofree

import Data.Dani


symbolParser :: P.Parser Symbol
symbolParser = symbol <$> P.takeWhile1 isAlpha  

stringParser :: P.Parser T.Text
stringParser = 
    P.char '"' *> P.takeWhile1 (\c -> not (c=='"'))  <* P.char '"' 

skipSpaceAndComma :: P.Parser ()
skipSpaceAndComma = P.skipWhile (\c -> isSpace c || c==',')

-------------------------------------------------------------------------------

parser_ :: P.Parser Dn_
parser_ = P.skipSpace *> elementParser_

elementParser_ :: P.Parser Dn_
elementParser_ = liftA (CofreeT . Identity . (:<) ()) $ 
    liftA Symbol symbolParser <|> 
    liftA String stringParser <|> 
    liftA Number P.scientific <|> 
    liftA List listParser_

listParser_ :: P.Parser [Dn_]
listParser_ = P.char '(' *> P.skipSpace *> P.manyTill elementParser_' (P.char ')')
  where
    elementParser_' = elementParser_ <* skipSpaceAndComma

-------------------------------------------------------------------------------

parser :: P.Parser Dn
parser = P.skipSpace *> elementParser

metadataListParser :: P.Parser [Dn_]
metadataListParser = many metadata 
  where
    metadata = P.char '^' *> parser_ <* P.skipSpace 

elemenWithMetadataParser :: P.Parser Dn
elemenWithMetadataParser = liftA2 
    (\metas -> CofreeT . env metas . extract . runCofreeT) 
    metadataListParser 
    elementParser

elementParser :: P.Parser Dn
elementParser = liftA (CofreeT . env [] . (:<) ()) $ 
    liftA Symbol symbolParser <|> 
    liftA String stringParser <|> 
    liftA Number P.scientific <|> 
    liftA List listParser

listParser :: P.Parser [Dn]
listParser = P.char '(' *> P.skipSpace *> P.manyTill elementWithMetadataParser' (P.char ')')
  where
    elementWithMetadataParser' = elemenWithMetadataParser <* skipSpaceAndComma

-------------------------------------------------------------------------------
