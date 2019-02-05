Note on free monads.

From category theory, monads can be thought of as monoid objects in the category of endofunctors of a 
category C. There is a forgetful functor from Mon(C), monoidal objects in any monoidal category, to C,
the underlying category. As \infty-categories, if C is bicomplete say, then limits in Mon(C) are
constructed in the underlying category (eg, products of monoids are set-theoretically the product). 
This implies the forgetful functor admits a left adjoint, given by the free-construction.

Hence, U : Monad(C) -> End(C) has a left adjoint, given by the free monad construction
      Free : End(C) -> Monad(C), f \mapsto Free f

As an example to illustrate free objects in functional programming, we use monoids.

< class Monoid m where
<     mempty :: m
<     mappend :: m -> m -> m

List are given as

< data [a] = [] | a : [a]

Given any type t we know that [t] is a monoid

< instance Monoid (List a) where
<     mempty  = []
<     mappend = (++)

The construction of a free monoid from a set in mathematics follows a standard pattern: The free monoid on
a set consists of all strings of characters of S with concatenation! This is indeed what List S is!

This makes sense mathematically-- we must be able to multiply two objects but otherwise we cannot say anything
as we impose no relations. So this should also work with monads. We need to somehow upgrade our functor to
something with a natural transformation m m -> m. But then the standard trick of letting
    Free f a = a + f a + f f a + f f f a + ... obviously has the right map by shifting.

But this can be written as Free f a = a + f (Free f a), so as a GADT we write it as

> data Free f a = Base a | Iter (f (Free f a))

This is actually a monad! To see this, lets fill out the algebraic instances:

> instance (Functor f) => Functor (Free f) where
>   fmap g (Base a) = Base (g a)
>   fmap g (Iter x) = Iter (fmap (fmap g) x)

What is the structure map in this case?

> joinFree :: Functor f => Free f (Free f a) -> Free f a
> joinFree (Base z) = z
> joinFree (Iter x) = Iter (fmap joinFree x)

This leads to the monad structure.

> instance (Functor f) => Monad (Free f) where
>   return = Base
>   x >>= g = joinFree (fmap g x)

> instance (Functor f) => Applicative (Free f) where
>   pure = return
>   g <*> x = do
>       gz <- g
>       xz <- x
>       return $ gz xz

From this we get our unit and counit for the adjunction: f a -> U Free f a and Free U m a -> m a

> counitFree :: Functor f => f a -> Free f a
> counitFree fa = Iter (fmap Base fa)

< unitFree :: Monad m => Free m a -> m a
< unitFree (Base a) = return a
< unitFree (Iter x) = ?

Should figure out how to write it at some point.