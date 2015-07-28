{-# LANGUAGE DeriveFunctor #-}

module Data.Dani.Types (
        Symbol
    ,   symbol
    ,   ValueF(..)
    ,   Value
    ,   Value_
    ) where

import qualified Data.Text as T
import qualified Data.Scientific as N
import Data.String
import Control.Comonad.Trans.Cofree
import Control.Comonad.Env

newtype Symbol = MakeSymbol T.Text deriving (Show, Eq)

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

type Value = CofreeT ValueF (Env [Value_]) ()

type Value_ = Cofree ValueF ()
