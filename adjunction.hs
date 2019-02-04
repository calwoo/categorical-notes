-- Come on Calvin, you know what adjunctions are!

import Prelude hiding (Functor, fmap)

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