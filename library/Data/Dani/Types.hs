{-# LANGUAGE DeriveFunctor #-}

module Data.Dani.Types (
        Symbol(asText)
    ,   symbol
    ,   ValueF(..)
    ) where

import qualified Data.Text as T
import qualified Data.Scientific as N
import Data.String
import Control.Comonad.Trans.Cofree
import Control.Comonad.Env

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

instance IsString (ValueF a) where
    fromString = String . T.pack

