{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Dani 
    ( 
        module Data.Dani.Types
    ,   Value
    ,   pattern Value
    ,   dropMeta
    ,   allowMeta
    ,   _meta
    ,   _anno
    ,   _data
    ,   _data'
    ,   _plate
    ,   ask
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

import Data.Dani.Lens

type Value m a = CofreeT ValueF (Env m) a

pattern Value a m d = CofreeT (EnvT m (Identity (a :< d)))

_meta :: Lens' (CofreeT g (Env e) a) e
_meta f (CofreeT (EnvT e x)) = CofreeT . flip EnvT x <$> f e

dropMeta :: Comonad w => CofreeT ValueF w a -> CofreeT ValueF (Env ()) a 
dropMeta = cohoist (EnvT () . Identity . extract) 

allowMeta :: (Comonad w, Monoid m) => CofreeT ValueF w a -> CofreeT ValueF (Env m) a
allowMeta = cohoist (env mempty . extract) 

_anno :: Lens' (CofreeT g (Env e) a) a
_anno f (CofreeT (EnvT e (Identity (a :< as)))) = CofreeT . EnvT e . Identity <$> (:< as) <$> f a

_data :: Lens' (CofreeT g (Env e) a) (g (CofreeT g (Env e) a))
_data f (CofreeT (EnvT e (Identity (a :< as)))) = CofreeT . EnvT e . Identity <$> (:<) a <$> f as

_data' :: Functor g => Iso' (CofreeT g (Env ()) ()) (g (CofreeT g (Env ()) ()))
_data' = iso unwrap (Value () ()) 

_plate :: (Traversable f, Traversable w) => Traversal' (CofreeT f w a) (CofreeT f w a)
_plate f (CofreeT xs) = CofreeT <$> traverse (traverse f) xs

#if !(MIN_VERSION_free(4,12,2))
instance Functor f => ComonadHoist (CofreeT f) where
    cohoist g = CofreeT . fmap (second (cohoist g)) . g . runCofreeT

instance (Functor f, ComonadEnv e w) => ComonadEnv e (CofreeT f w) where
    ask = ask . lower
    {-# INLINE ask #-}
#endif

