{-# LANGUAGE CPP #-}
module Data.Dani 
    ( 
        module Data.Dani.Types
    ,   dropMeta
    ,   allowMeta
    ,   getMeta
    ,   wrap0    
    ,   wrap
    ,   wrap_  
    ,   unwrap
    ,   cohoist
    ) where 

import Data.Dani.Types

import Data.Monoid
#if !(MIN_VERSION_free(4,12,2))
import Data.Bifunctor
#endif
import Data.Functor.Identity
import Control.Comonad.Trans.Cofree
import Control.Comonad.Env
import Control.Comonad.Hoist.Class

wrap0 :: (Monoid a, Applicative w) 
      => ValueF (CofreeT ValueF w a)
      -> CofreeT ValueF w a
wrap0 v = CofreeT $ pure $ mempty :< v

wrap :: a 
         -> e 
         -> ValueF (CofreeT ValueF (Env e) a)
         -> CofreeT ValueF (Env e) a
wrap a e = CofreeT . env e . (:<) a

wrap_ :: a -> ValueF (Cofree ValueF a) -> Cofree ValueF a
wrap_ a = cofree . (:<) a

getMeta :: ComonadTrans t => t (Env e) x -> e
getMeta = ask . lower

dropMeta :: Comonad w => CofreeT ValueF w a -> Cofree ValueF a 
dropMeta = cohoist (Identity . extract) 

allowMeta :: Monoid m => Cofree ValueF a -> CofreeT ValueF (Env m) a
allowMeta = cohoist (env mempty . runIdentity) 

#if !(MIN_VERSION_free(4,12,2))
instance Functor f => ComonadHoist (CofreeT f) where
    cohoist g = CofreeT . fmap (second (cohoist g)) . g . runCofreeT
#endif

