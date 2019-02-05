-- a quick hashing out of categorical structures in Haskell code

{-# LANGUAGE PolyKinds, TypeOperators, FlexibleInstances, RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts #-}

import Prelude hiding (Functor, fmap, (.), Monad, id)
import GHC.Exts (Constraint)

-- categories.

-- somehow we need to represent the set of morphisms in a category. We could let categories
-- themselves be type constructors of kind k -> k -> * , ie, all we really care about is that
-- categories are enriched over sets and that the only data they contain is their hom sets

type (a ~> b) c = c a b -- given category c, the morphism set from "objects" a to b

class Category (c :: k -> k -> *) where -- here, kind * are "sets"
    id :: (a ~> a) c -- for all objects a, there is an identity map
    (.) :: (y ~> z) c -> (x ~> y) c -> (x ~> z) c -- composition is an enriched map of morphism sets

-- We can give an actual realization of our hom sets via the Hask category
type Hask = (->)

instance Category Hask where
    id x = x
    -- (.) :: (y -> z) -> (x -> y) -> (x -> z)
    (f . g) x = f (g x)


-- functors.
class (Category c, Category d) => Functor c d t where
    -- fmap :: (a ~> b) c -> (t a ~> t b) d
    fmap :: c a b -> d (t a) (t b)

-- functor laws:
--    a) fmap id = id
--    b) (fmap f) . (fmap g) = fmap (f . g)

-- examples)
newtype Id a = Id a

instance Functor Hask Hask Id where
    fmap f (Id x) = Id (f x)

instance Functor Hask Hask [] where
    fmap f [] = []
    fmap f (x:xs) = (f x) : (fmap f xs)

instance Functor Hask Hask Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

-- endofunctors.
type Endofunctor c t = Functor c c t

-- composition of functors are functors.
newtype FunctComp g f x = Comp { unComp :: g (f x) }
-- TODO: figure out why

{- instance (Functor a b f, Functor b c g) => Functor a c (FunctComp g f) where
    fmap f = fmapg $ fmapf f
        where
            fmapf = fmap :: a x y -> b (f x) (f y)
            fmapg = fmap :: b s t -> c (g s) (g t)
        -- annoyingly, this won't work, as we somehow have to wrap c (g f x) (g f y) into
        -- c (Comp g (f x)) (Comp g (f y)) -}

-- gotta do some hack to decouple the type parameters
newtype Hom (c :: * -> Constraint) a b = Hom (a -> b) -- this is a wrapper for the codomain category

test = Hom unComp

instance (Functor a b f, Functor b c g, c ~ Hom k) => Functor a c (FunctComp g f) where
    fmap f = (Hom Comp) . fmapg (fmapf f) . (Hom unComp)
      where
        fmapf = fmap :: a x y -> b (f x) (f y)
        fmapg = fmap :: b s t -> c (g s) (g t)

-- natural transformations.
type Nat c f g = forall a. c (f a) (g a)
type NatHask f g = forall a. (f a) -> (g a)

-- example)
headMay :: forall a. [a] -> Maybe a
headMay [] = Nothing
headMay (a:as) = Just a

-- functor categories.
newtype Fun f g a b = FNat (f a -> g b)

-- endofunctors
type End f = Fun f f
instance Category (End f) where
    id = FNat id
    (FNat f) . (FNat g) = FNat (f . g)

-- monads.
class Endofunctor c t => Monad c t where
    eta :: c a (t a)
    mu :: c (t (t a)) (t a)

(>>=) :: (Monad c t) => c a (t b) -> c (t a) (t b)
(>>=) f = mu . fmap f

return :: (Monad c t) => c a (t a)
return = eta

-- Kleisli category.
-- every monad gives rise to a category, the kleisli category.

newtype Kleisli c t a b = K (c a (t b))

type (a :~> b) c t = Kleisli c t a b
-- composition in a Kleisli category given by fish operator
(<=<) :: (Monad c t) => c y (t z) -> c x (t y) -> c x (t z)
f <=< g = mu . fmap f . g

instance Monad c t => Category (Kleisli c t) where
    id = K eta
    (K f) . (K g) = K (f <=< g)

-- example) Hask! Here, c = (->)

newtype KleisliHask m a b = KHask (a -> m b)

type (a ::~> b) m = KleisliHask m a b

(<=<<) :: (Monad Hask m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=<< g = mu . fmap f . g

instance Monad Hask m => Category (KleisliHask m) where
    id = KHask eta
    (KHask f) . (KHask g) = KHask (f <=<< g)

-- in Hask, functors and monads are of the form

{- 
class Functor t where
    fmap :: (a -> b) -> t a -> t b

class Functor t => Monad t where
    eta :: a -> t a
    mu :: t (t a) -> t a

(>>=) :: Monad t => t a -> (a -> t b) -> t b
tx >>= f = join . fmap f tx
-}

