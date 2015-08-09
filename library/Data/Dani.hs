{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Dani 
    ( 
        module Data.Dani.Types
    ,   Value
    ,   Value_
    ,   pattern Value
    ,   dropMeta
    ,   allowMeta
    ,   _meta
    ,   _anno
    ,   _data
    ,   ask
    ,   asks
    ,   extract
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
import Control.Comonad.Env.Class
import Control.Comonad.Hoist.Class

type Value m a = CofreeT ValueF (Env m) a

type Value_ a = CofreeT ValueF (Env ()) a

pattern Value a m d = CofreeT (EnvT m (Identity (a :< d)))

_meta :: Functor f => (e -> f e) -> CofreeT g (Env e) a -> f (CofreeT g (Env e) a)
_meta f (CofreeT (EnvT e x)) = CofreeT . flip EnvT x <$> f e

dropMeta :: Comonad w => CofreeT ValueF w a -> CofreeT ValueF (Env ()) a 
dropMeta = cohoist (EnvT () . Identity . extract) 

allowMeta :: (Comonad w, Monoid m) => CofreeT ValueF w a -> CofreeT ValueF (Env m) a
allowMeta = cohoist (env mempty . extract) 

_anno :: (Traversable w, Applicative f) => (a -> f a) -> CofreeT g w a -> f (CofreeT g w a)
_anno f (CofreeT w) = CofreeT <$> traverse (\(a :< as) -> (:< as) <$> f a) w

_data :: (Traversable w, Applicative f) 
      => (g (CofreeT g w a) -> f (g (CofreeT g w a))) 
      -> CofreeT g w a -> f (CofreeT g w a)
_data f (CofreeT w) = CofreeT <$> traverse (\(a :< as) -> (:<) a <$> f as)  w

#if !(MIN_VERSION_free(4,12,2))
instance Functor f => ComonadHoist (CofreeT f) where
    cohoist g = CofreeT . fmap (second (cohoist g)) . g . runCofreeT

instance (Functor f, ComonadEnv e w) => ComonadEnv e (CofreeT f w) where
    ask = ask . lower
    {-# INLINE ask #-}
#endif

