-- Functional dependencies note

-- Functional dependencies are used to constrain the parameters of type classes. They allow you to say that
-- in a multiparameter type class, some of the parameters are constrained by others.

-- example)

data Vector = Vector Int Int
    deriving (Eq, Show)

data Matrix = Matrix Vector Vector deriving (Eq, Show)

-- We have vectors and 2x2 matrices and we want them to act like numbers as much as possible, so
-- we start overloading the Num instance. However, the hard part is multiplication. We would need to
-- normally specify a bunch of different operators!

-- Can we combine them into one type class?

class Mult a b c where
    (*) :: a -> b -> c

-- Is this what we want?
instance Mult Matrix Matrix Matrix where
    <blah blah>

instance Mult Matrix Vector Matrix where
    <blah blah>

-- But this is ambigious for the type system. Is it clear that a Matrix * Matrix is a matrix?
-- after all, you could have

instance Mult Matrix Matrix (Maybe Char) where
    <blah blah>

-- This is crazy! In math, the input types tend to determine the return type. Here, the typevar c
-- shouldn't be free. So we add in a functional dependency to constrain it:

class Mult a b c | a b -> c where
    (*) :: a -> b -> c

-- So this means that once we have an instance Mult Matrix Matrix Matrix, as soon as we define the
-- instance Mult Matrix Matrix (Maybe Char), GHC will throw an error, because we have constrained the
-- last typevar.