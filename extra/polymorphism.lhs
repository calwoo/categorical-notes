> {-# LANGUAGE RankNTypes #-}

A polymorphic function is one that works for many types.

< hd :: [a] -> Maybe a

or with multiple typevars, 

< map :: a -> b -> [a] -> [b]

We can be pretty explicit about the fact that `a, b` could be any type by using existential quantification:

< hd :: forall a. [a] -> a

### Higher rank types

Now we can write functions that expect polymorphic arguments:

> foo :: (forall a. a -> a) -> (Char, Bool)
> foo f = (f 'c', f Bool)

Here, f is a polymorphic function, which means it can apply to any type.
Note: this is different from

< bar :: forall a. ((a -> a) -> (Char, Bool)) !

In this case, we say foo is a rank-2 type. A general rank-n type is a type with a rank-(n-1) type in its
arguments.