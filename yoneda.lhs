> {-# LANGUAGE RankNTypes #-}

The Yoneda lemma says something fairly obvious in mathematics, but in a formal-categorical way:
"If you want to know something well, it suffices to know maps out of that object."

That is to say, given a functor F:C->Set, the set of natural transformations from h_A to F is
naturally equivalent to F(A).

In Haskell, this can be written

< (forall b. (a -> b) -> f b) == f a

This can be proved by given an explicit pair of inverse maps

> check :: Functor f => f a -> (forall b. (a -> b) -> f b)
> check a f = fmap f a

and 

> uncheck :: Functor f => (forall b. (a -> b) -> f b) -> f a
> uncheck t = t id

These are obvious inverse by definition as

< uncheck (check a) = check a id = fmap id a = a

and

< check (uncheck t) f = fmap f (uncheck t) = fmap f (t id) = t f

