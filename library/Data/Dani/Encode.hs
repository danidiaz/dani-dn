{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Dani.Encode (
        module Data.Dani
    ) where


import Data.Dani
import Data.Monoid
import Data.Foldable
import qualified Data.List.NonEmpty as S
import Data.Text.Lazy.Builder.Scientific
import qualified Data.Text.Lazy.Builder as T
import Control.Comonad.Trans.Cofree

space :: T.Builder
space = T.singleton ' '

meta :: T.Builder
meta = T.singleton '^'

lparen :: T.Builder  
lparen = T.singleton '('

rparen :: T.Builder  
rparen = T.singleton '('

encodeToTextBuilder :: Value -> T.Builder
encodeToTextBuilder = undefined

encodeToTextBuilder_ :: Value_ -> T.Builder
encodeToTextBuilder_ v_ = case unwrap v_ of 
    String s -> T.fromText s
    Number n -> scientificBuilder n 
    List [] -> lparen <> rparen
    List [x] -> lparen <> encodeToTextBuilder_ x <> rparen
    List l@(x:y:zs) -> 
        let (pairs,lastz) = undefined 
        in
        fold $ 
            [
              lparen 
            , foldMap ((`mappend` space) . encodeToTextBuilder_ . fst) pairs
            , encodeToTextBuilder_ lastz 
            , rparen  
            ]



