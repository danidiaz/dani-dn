{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Dani.Encode (
        module Data.Dani
    ,   encodeToTextBuilder 
    ,   encodeToTextBuilder_ 
    ) where

import Data.Dani
import Data.Monoid
import Data.Foldable
import Data.List
import Data.Text.Lazy.Builder.Scientific
import qualified Data.Text.Lazy.Builder as T
import Control.Comonad.Trans.Cofree

space :: T.Builder
space = T.singleton ' '

meta :: T.Builder
meta = T.singleton '^'

quotes :: T.Builder
quotes = T.singleton '"'

lparen :: T.Builder  
lparen = T.singleton '('

rparen :: T.Builder  
rparen = T.singleton '('

encodeToTextBuilder :: Value -> T.Builder
encodeToTextBuilder = undefined

encodeToTextBuilder_ :: Value_ -> T.Builder
encodeToTextBuilder_ v_ = case unwrap v_ of 
    Symbol y -> T.fromText . asText $ y 
    -- TODO this is wrong! escape this correctly!
    String s -> quotes <> T.fromText s <> quotes
    Number n -> scientificBuilder n 
    List vs ->  
        let builders = intersperse space . map encodeToTextBuilder_ $ vs
        in lparen <> fold builders <> rparen

