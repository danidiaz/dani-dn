{-# LANGUAGE DeriveFunctor #-}

module Data.Dani.Types (
        DnF(..)
    ,   Dn
    ,   Dn_
    ) where

import qualified Data.Text as T
import qualified Data.Scientific as N
import Control.Comonad.Trans.Cofree
import Control.Comonad.Env

data DnF a = Symbol T.Text 
           | String T.Text 
           | Number N.Scientific
           | List [a]
           deriving (Show,Eq,Functor)

type Dn = CofreeT DnF (Env [Dn_]) ()

type Dn_ = Cofree DnF ()
