{-# LANGUAGE CPP #-}
module Data.Dani 
    ( 
        module Data.Dani.Types
    ,   dropMeta
    ,   allowMeta
    ,   _meta
    ,   wrap0    
    ,   wrap
    ,   wrap_  
    ,   _a
    ,   _v
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

_meta :: Functor f => (e -> f e) -> CofreeT g (Env e) a -> f (CofreeT g (Env e) a)
_meta f (CofreeT (EnvT e x)) = CofreeT . flip EnvT x <$> f e

dropMeta :: Comonad w => CofreeT ValueF w a -> Cofree ValueF a 
dropMeta = cohoist (Identity . extract) 

allowMeta :: Monoid m => Cofree ValueF a -> CofreeT ValueF (Env m) a
allowMeta = cohoist (env mempty . runIdentity) 

_a :: (Traversable w, Applicative f) => (a -> f a) -> CofreeT g w a -> f (CofreeT g w a)
_a f (CofreeT w) = CofreeT <$> traverse (\(a :< as) -> (:< as) <$> f a) w

_v :: (Traversable w, Applicative f) 
   => (g (CofreeT g w a) -> f (g (CofreeT g w a))) 
   -> CofreeT g w a -> f (CofreeT g w a)
_v f (CofreeT w) = CofreeT <$> traverse (\(a :< as) -> (:<) a <$> f as)  w

#if !(MIN_VERSION_free(4,12,2))
instance Functor f => ComonadHoist (CofreeT f) where
    cohoist g = CofreeT . fmap (second (cohoist g)) . g . runCofreeT
#endif

