data Free f a = Base a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
    fmap f (Base a) = Base (f a)
    fmap f (Free as) = Free (fmap (fmap f) as)

instance Functor f => Monad (Free f) where
    return = Base
    (Base a) >>= f = f a
    (Free fa) >>= f = Free (fmap (>>= f) fa)
    
-- need applicative as well
instance Functor f => Applicative (Free f) where
    pure = return
    f <*> x = x >>= (\z -> f >>= (\g -> return $ g z))

