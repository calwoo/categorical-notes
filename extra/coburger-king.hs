-- Reading notes from EZYang's "Flipping arrows in coBurger King"
        -- http://blog.ezyang.com/2010/07/flipping-arrows-in-coburger-king/

import Prelude hiding (Monad)


class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a

class Comonad w where
    (=>>) :: w a -> (w a -> b) -> w b
    extract :: w a -> a

-- the definition of monad in terms of >>= makes it pretty hard to see the co- in comonad
-- solution: write it in terms of the monadic join

-- first we need some preliminaries on the category Hask. to dualize things, we need to understand Hask^op

{- class Category (~>) where
    id :: a ~> a
    (.) :: (b ~> c) -> (a ~> b) -> (a ~> c) -}

class ContraFunctor t where
    contramap :: (a -> b) -> t b -> t a

newtype ContraF a b = ContraF (b -> a)
instance ContraFunctor (ContraF a) where
    contramap g (ContraF h) = ContraF $ h . g

class Functor m => MonadC m where
    join :: m (m a) -> m a
    returnC :: a -> m a

-- this makes it clear how to flip the arrows

class Functor w => ComonadC w where
    cojoin :: w a -> w (w a)
    coreturn :: w a -> a

