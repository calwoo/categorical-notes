-- All concepts are Kan extensions.

{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

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





-- Dually, a left Kan extension is the colimit over the category of maps (g a -> b) of the h b.