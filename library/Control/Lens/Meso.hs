{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Lens.Meso (
        Lens
    ,   Lens'
    ,   Traversal
    ,   Traversal'
    ,   Prism
    ,   Prism'
    ,   Iso
    ,   Iso'
    ,   to
    ,   view
    ,   preview
    ,   toListOf 
    ,   foldrOf 
    ,   foldMapOf 
    ,   over
    ,   re
    ,   prism
    ,   prism'
    ,   below
    ,   iso
    ,   from
    ,   mapping
    ,   universeOf
    ,   paraOf
    ,   transformOf
    ) where

import Data.Functor.Identity
import Data.Profunctor
import Data.Monoid
import Data.Tagged
import Data.Functor.Contravariant
import Control.Applicative

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

type Traversal' s a = Traversal s s a a

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

type Iso' s a = Iso s s a a

type AReview t b = Tagged b (Identity b) -> Tagged t (Identity t)

type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

type LensLike f s t a b = (a -> f b) -> s -> f t

type LensLike' f s a = LensLike f s s a a

----------
--- Getter

type Getting r s a = (a -> Const r a) -> s -> Const r s

type Accessing p m s a = p a (Const m a) -> s -> Const m s

to :: (Profunctor p, Contravariant f) => (s -> a) -> p a (f a) -> p s (f s)
to k = dimap k (contramap k)

view :: Getting a s a -> s -> a
view l s = (getConst . l Const) s

preview :: Getting (First a) s a -> s -> (Maybe a)
preview l s = (getFirst . foldMapOf l (First . Just)) s

toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = foldrOf l (:) []

foldrOf :: Accessing (->) (Endo r) s a -> (->) a (r -> r) -> r -> s -> r
foldrOf l f z = flip appEndo z `rmap` foldMapOf l (Endo . f)
{-# INLINE foldrOf #-}

foldMapOf :: Accessing (->) r s a -> (->) a r -> s -> r
foldMapOf l f = getConst . l (Const . f)

-------------------
--- Setter

over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

-------------------
--- Prism internals

data Market a b s t = Market (b -> t) (s -> Either t a)

-- | @type 'Market'' a s t = 'Market' a a s t@
type Market' a = Market a a

instance Functor (Market a b s) where
  fmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE fmap #-}

instance Profunctor (Market a b) where
  dimap f g (Market bt seta) = Market (g . bt) (either (Left . g) Right . seta . f)
  {-# INLINE dimap #-}
  lmap f (Market bt seta) = Market bt (seta . f)
  {-# INLINE lmap #-}
  rmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE rmap #-}

instance Choice (Market a b) where
  left' (Market bt seta) = Market (Left . bt) $ \sc -> case sc of
    Left s -> case seta s of
      Left t -> Left (Left t)
      Right a -> Right a
    Right c -> Left (Right c)
  {-# INLINE left' #-}
  right' (Market bt seta) = Market (Right . bt) $ \cs -> case cs of
    Left c -> Left (Left c)
    Right s -> case seta s of
      Left t -> Left (Right t)
      Right a -> Right a
  {-# INLINE right' #-}


-------------------
--- Prism functions

type APrism s t a b = Market a b a (Identity b) -> Market a b s (Identity t)

type APrism' s a = APrism s s a a

withPrism :: APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
withPrism k f = case k (Market Identity Right) of
  Market bt seta -> f (runIdentity . bt) (either (Left . runIdentity) Right . seta)

re :: Contravariant f => AReview t b -> LensLike' f b t
re p = to (runIdentity . unTagged . p . Tagged . Identity)

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))

below :: Traversable f => APrism' s a -> Prism' (f s) (f a)
below k =
  withPrism k     $ \bt seta ->
  prism (fmap bt) $ \s ->
  case traverse seta s of
    Left _  -> Left s
    Right t -> Right t

-----------------
--- Iso internals

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Functor (Exchange a b s) where
  fmap f (Exchange sa bt) = Exchange sa (f . bt)
  {-# INLINE fmap #-}

instance Profunctor (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)
  {-# INLINE dimap #-}
  lmap f (Exchange sa bt) = Exchange (sa . f) bt
  {-# INLINE lmap #-}
  rmap f (Exchange sa bt) = Exchange sa (f . bt)
  {-# INLINE rmap #-}


-----------------
--- Iso functions

type AnIso s t a b = Exchange a b a (Identity b) -> Exchange a b s (Identity t)

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)

from :: AnIso s t a b -> Iso b a t s
from l = withIso l $ \ sa bt -> iso bt sa
{-# INLINE from #-}

withIso :: AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k = case ai (Exchange id Identity) of
  Exchange sa bt -> k sa (runIdentity . bt)

mapping :: (Functor f, Functor g) => AnIso s t a b -> Iso (f s) (g t) (f a) (g b)
mapping k = withIso k $ \ sa bt -> iso (fmap sa) (fmap bt)

-------
-- Plated

universeOf :: Getting [a] a a -> a -> [a]
universeOf l = go where
  go a = a : foldMapOf l go a

paraOf :: Getting (Endo [a]) a a -> (a -> [r] -> r) -> a -> r
paraOf l f = go where
  go a = f a (go <$> toListOf l a)

transformOf :: Traversal' a a -> (a -> a) -> a -> a
transformOf l f = go where
  go = f . over l go
{-# INLINE transformOf #-}
