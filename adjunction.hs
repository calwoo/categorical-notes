-- Come on Calvin, you know what adjunctions are!

{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

import Prelude hiding (Functor, fmap, Monad)

class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- functor composition in Hask (oh god, this is easier)
newtype FunctComp f g a = Comp { unComp :: f (g a) }
instance (Functor f, Functor g) => Functor (FunctComp f g) where
    fmap k (Comp fgx ) = Comp (fmap (fmap k) fgx)

newtype Id a = Identity { unId :: a }

instance Functor Id where
    fmap f (Identity x) = Identity (f x)

-- composition with the identity is trivial
left :: Functor f => FunctComp f Id a -> f a
left x = fmap unId (unComp x)

right :: Functor f => f a -> FunctComp f Id a
right z = Comp $ fmap Identity z

-- natural transformations in Hask are given by polymorphic functions
type Nat f g = forall a. f a -> g a
-- this turns the category of categories into a 2-category

type NatComp f f' g g' = forall a. f' (f a) -> g' (g a)

-- vertical composition
vert :: (Functor f, Functor f', Functor g, Functor g') =>
    Nat f' g' -> Nat f g -> NatComp f f' g g'
vert a b x = a (fmap b x)

horiz :: (Functor f, Functor f', Functor g, Functor g') =>
    Nat f' g' -> Nat f g -> NatComp f f' g g'
horiz a b x = fmap b (a x)

-- we have the Eckmann-Hilton argument
-- (a . b) `vert` (a' . b') = (a `horiz` a') . (b `horiz` b')

-- adjunctions.

class (Functor f, Functor g) => Adjoint f g | f -> g, g -> f where
    eta :: a -> g (f a) -- here, g is the right adjoint; this is the counit
    epsilon :: f (g a) -> a -- this is the unit

-- adjunctions can also be defined by an equivalence of hom-sets
phi :: Adjoint f g => (f a -> b) -> (a -> g b)
phi k = fmap k . eta

phiInv :: Adjoint f g => (a -> g b) -> (f a -> b)
phiInv k = epsilon . fmap k

-- an adjunction that all mathematicians know but apparently computer scientists seem to freak
-- out about is the tensor-hom adjunction, which they call "currying".
instance Functor ((,) a) where
    fmap f (x,y) = (x, f y)

instance Functor ((->) a) where
    fmap f k = f . k

instance Adjoint ((,) a) ((->) a) where
    -- eta :: b -> a -> (a, b)
    eta x y = (y, x)
    -- epsilon :: (a, (a -> b)) -> b
    epsilon (x, f) = f x

-- =================
-- MONADS

class (Functor t) => Monad t where
    etaM :: a -> t a
    mu :: t (t a) -> t a
    (>>=) :: t a -> (a -> t b) -> t b
    x >>= f = mu $ fmap f x

-- A standard fact of category theory is that every adjoint pair of functors give rise to a monad.
-- lets do this in Hask:

{- class Adjoint f g => Monad f g where
    muM :: g (f (g (f a))) -> g (f a)
    muM = fmap epsilon

    etaM' :: a -> g (f a)
    etaM' = eta

    (>>=) :: g (f a) -> (a -> g (f b)) -> g (f b)
    x >>= f = muM $ fmap (fmap f x) -}

