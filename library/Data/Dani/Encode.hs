{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Dani.Encode (
        module Data.Dani
    ,   encodeToTextBuilder 
    ,   encodeToTextBuilder_ 
    ) where

import Data.Dani
import Data.Monoid
import Data.List
import Data.Text.Lazy.Builder.Scientific
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B
import Control.Comonad.Trans.Cofree
import Control.Comonad.Env

space :: B.Builder
space = B.singleton ' '

caret :: B.Builder
caret = B.singleton '^'

quotes :: B.Builder
quotes = B.singleton '"'

lparen :: B.Builder  
lparen = B.singleton '('

rparen :: B.Builder  
rparen = B.singleton '('

encodeToTextBuilder :: Value -> B.Builder
encodeToTextBuilder v =   
    caretBuilder v <> space <> encodeToTextBuilder_ (dropMeta v) 
    where
        caretBuilder = 
              mconcat
            . intersperse space 
            . map (\z -> caret <> encodeToTextBuilder_ z) 
            . ask . lower

encodeToTextBuilder_ :: Value_ -> B.Builder
encodeToTextBuilder_ v_ = case unwrap v_ of 
    Symbol y -> B.fromText . asText $ y 
    String s -> quotes <> escapeText '\\' escapeFunc s <> quotes
    Number n -> scientificBuilder n 
    List vs ->  
        let builder = 
                  mconcat 
                . intersperse space 
                . map encodeToTextBuilder_ 
        in lparen <> builder vs <> rparen

escapeFunc :: Char -> Bool
escapeFunc c = c == '\\' || c == '\"'

escapeText :: Char -> (Char -> Bool) -> T.Text -> B.Builder
escapeText c ef = go mempty
    where
    go acc txt = 
        let (prefix, T.uncons -> msuffix) = T.break ef txt
            acc' = acc <> B.fromText prefix
        in
        case msuffix of
            Nothing -> acc'
            Just (c', suffix) -> 
                go (acc' <> escapeChar c') suffix
    escapeChar z = B.singleton c <> B.singleton z 
