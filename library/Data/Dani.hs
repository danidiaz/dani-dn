{-# LANGUAGE CPP #-}
module Data.Dani 
    ( 
        module Data.Dani.Types
    ,   value
    ,   value_
    ,   dropMeta
    ,   allowMeta
    ) where 

import Data.Dani.Types

#if !(MIN_VERSION_free(4,12,2))
import Data.Bifunctor
#endif
import Data.Functor.Identity
import Control.Comonad.Trans.Cofree
import Control.Comonad.Env
import Control.Comonad.Hoist.Class

value :: a 
      -> e 
      -> ValueF (CofreeT ValueF (Env e) a)
      -> CofreeT ValueF (Env e) a
value a e = CofreeT . env e . (:<) a

value_ :: a -> ValueF (Cofree ValueF a) -> Cofree ValueF a
value_ a = cofree . (:<) a

dropMeta :: Comonad w => CofreeT ValueF w a -> Cofree ValueF a 
dropMeta = cohoist (Identity . extract) 

allowMeta :: Monoid m => Cofree ValueF a -> CofreeT ValueF (Env m) a
allowMeta = cohoist (env mempty . runIdentity) 

#if !(MIN_VERSION_free(4,12,2))
instance Functor f => ComonadHoist (CofreeT f) where
    cohoist g = CofreeT . fmap (second (cohoist g)) . g . runCofreeT
#endif

