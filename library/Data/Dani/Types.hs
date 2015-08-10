{-# LANGUAGE DeriveFunctor #-}

module Data.Dani.Types (
        Symbol(asText)
    ,   symbol
    ,   ValueF(..)
    ,   _Symbol
    ,   _String
    ,   _Number
    ,   _List
    ,   _BoundedInteger 
    ,   _RealFloat 
    ,   _atomic
    ) where

import qualified Data.Text as T
import qualified Data.Scientific as N
import Data.String
import Data.Void
import Control.Comonad.Trans.Cofree
import Control.Comonad.Env

import Data.Dani.Lens

newtype Symbol = MakeSymbol { asText :: T.Text } deriving (Show, Eq) 

instance IsString Symbol where
    fromString = MakeSymbol . T.pack

symbol :: T.Text -> Symbol 
symbol = MakeSymbol

data ValueF a = Symbol Symbol
              | String T.Text 
              | Number N.Scientific
              | List [a]
              deriving (Show,Eq,Functor)

_Symbol :: Prism' (ValueF a) Symbol
_Symbol = prism' Symbol deconstruct
    where
    deconstruct v = case v of
        Symbol s -> Just s
        _ -> Nothing

_String :: Prism' (ValueF a) T.Text
_String = prism' String deconstruct
    where
    deconstruct v = case v of
        String s -> Just s
        _ -> Nothing

_Number :: Prism' (ValueF a) N.Scientific
_Number = prism' Number deconstruct
    where
    deconstruct v = case v of
        Number s -> Just s
        _ -> Nothing

_List :: Prism' (ValueF a) [a]
_List = prism' List deconstruct
    where
    deconstruct v = case v of
        List as -> Just as
        _ -> Nothing

_BoundedInteger :: (Integral i, Bounded i) => Prism' N.Scientific i
_BoundedInteger = prism' (\i -> N.scientific (toInteger i) 1) N.toBoundedInteger

_RealFloat :: RealFloat f => Iso' N.Scientific f
_RealFloat =  iso N.toRealFloat N.fromFloatDigits 

_atomic :: Prism' (ValueF a) (ValueF Void) 
_atomic = prism' construct deconstruct
    where
    deconstruct v = case v of
        Symbol s -> Just $ Symbol s
        String str -> Just $ String str
        Number n -> Just $ Number n
        List [] -> Just $ List []
        List (_:_) -> Nothing
    construct v = fmap absurd v

instance IsString (ValueF a) where
    fromString = String . T.pack
