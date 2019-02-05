-- All concepts are Kan extensions.

{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Monad.Identity

-- RIGHT KAN EXTENSIONS:

newtype Ran g h a = Ran 
    { runRan :: forall b. (a -> g b) -> h b }

-- Ran g h is the right Kan extension of h along g. The above definition makes sense:
-- a right Kan extension is the limit over the category of maps (a -> g b) of the h b.

-- NOTE: Even without knowing what g h are, Ran g f is a functor!
instance Functor (Ran g h) where
    fmap f m = Ran (\k -> runRan m (k . f))

-- YONEDA LEMMA:
-- The natural isomorphism given by the two maps
check :: Functor f => f a -> (forall b. (a -> b) -> f b)
check a f = fmap f a

uncheck :: Functor f => (forall b. (a -> b) -> f b) -> f a
uncheck t = t id

-- Note then that the yoneda embedding (forall b. (a -> b) -> f b) can be described by
type Yoneda = Ran Identity
-- this makes sense! the right Kan extension of a functor along the identity is itself!

-- ADJUNCTIONS:
-- Lets relate right Kan extensions to adjunctions!
class (Functor f, Functor g) => Adjoint f g | f -> g, g -> f where
    -- we impose these functional dependencies because when one writes fmap, it is unclear
    -- which functor it belongs to.
    unit :: a -> g (f a)
    counit :: f (g a) -> a
    leftAdj :: (f a -> b) -> (a -> g b)
    rightAdj :: (a -> g b) -> (f a -> b)

-- a fact of category theory is that if f g is an adjunction, then f can be given as the right kan
-- extension of the identity along g

adjointToRan :: Adjoint f g => f a -> Ran g Identity a
adjointToRan x = Ran $ \k -> Identity $ rightAdj k $ x
-- k :: a -> g b
ranToAdjoint :: Adjoint f g => Ran g Identity a -> f a
ranToAdjoint k = runIdentity $ runRan k unit

-- similarly we get a natural isomorphism between g and Lan f Identity
-- Here, dually, a left Kan extension is the colimit over the category of maps (g a -> b) of the h b.

data Lan g h a = forall b. Lan (g b -> a) (h b)

adjointToLan :: Adjoint f g => g a -> Lan f Identity a
adjointToLan = Lan counit . Identity

lanToAdjoint :: Adjoint f g => Lan f Identity a -> g a
lanToAdjoint (Lan f v) = leftAdj f (runIdentity v)

-- COLIMITS AND LIMITS:
-- finally, we use Kan extensions to get limits and colimits in Hask

-- we know that categorically, Kan extensions along the trivial functor gives our (co)limits

data Trivial a = Trivial

instance Functor Trivial where
    fmap f _ = Trivial

trivialize :: a -> Trivial b
trivialize _ = Trivial

-- a limit is a right Kan extension along the trivial functor

type Lim = Ran Trivial

-- so now we can say
-- Lim f a = forall b. (a -> Trivial b) -> f b

-- examples)
    -- Lim Maybe a = Nothing
    -- Lim [] a = []

-- a colimit is a left Kan extension along the trivial functor

type Colim = Lan Trivial

-- so now we can say
-- Colim f a = exists b. (Trivial b -> a, f b)

-- CODENSITY MONAD:
-- we can take the right Kan extension of a functor g along itself and get a monad!

instance Monad (Ran g g) where
    return x = Ran $ \k -> k x
    m >>= k = Ran $ \s -> runRan m $ (\x -> runRan (k x) s)

instance Applicative (Ran g g) where
    pure = return
    f <*> x = do
        af <- f
        ax <- x
        return (af ax)