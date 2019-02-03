-- a quick hashing out of categorical structures in Haskell code

{-# LANGUAGE PolyKinds, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
