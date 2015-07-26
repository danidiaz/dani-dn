{-# LANGUAGE OverloadedStrings #-}

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

-- . * + ! - _ ? $ % & = < >

isFirst :: Char -> Bool
isFirst c = 
    (isAlpha c) ||
    (c == '*') || 
    (c == '!') || 
    (c == '_') || 
    (c == '?') || 
    (c == '$') || 
    (c == '%') || 
    (c == '&') || 
    (c == '=') || 
    (c == '<') || 
    (c == '>') 

isSpecialFirst :: Char -> Bool
isSpecialFirst c = 
    (c == '.') || 
    (c == '+') || 
    (c == '-')

isSpecial :: Char -> Bool
isSpecial c = 
    (c == '.') || 
    (c == '*') || 
    (c == '+') || 
    (c == '!') || 
    (c == '-') || 
    (c == '_') || 
    (c == '?') || 
    (c == '$') || 
    (c == '%') || 
    (c == '&') || 
    (c == '=') || 
    (c == '<') || 
    (c == '>') 

isMiddle :: Char -> Bool
isMiddle c = isAlphaNum c || isSpecial c

isSpecialMiddle :: Char -> Bool
isSpecialMiddle c = isAlpha c || isSpecial c

symbolParser :: P.Parser Symbol
symbolParser = liftA symbol $ 
    liftA2 T.cons (P.satisfy isFirst) (P.takeWhile isMiddle) <|>
    liftA3 (\c1 c2 t -> T.cons c1 (T.cons c2 t)) (P.satisfy isSpecialFirst) (P.satisfy isSpecialMiddle) (P.takeWhile isMiddle) <|>
    liftA T.singleton (P.satisfy isSpecialFirst) <|>
    (P.char '/' *> pure "/")

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
    liftA Number P.scientific <|> -- Number should go before Symbol
    liftA String stringParser <|> 
    liftA Symbol symbolParser <|> 
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
