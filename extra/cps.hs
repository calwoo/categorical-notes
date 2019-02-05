-- Continuation-passing style (CPS) is a style of programming where functions do not return values, but pass
-- control onto a "continuation" which specifies what happens next.

-- example)

x = map ($ 2) [(2*), (3*), (5*), (7*)]
y = map (*2) [2, 3, 5, 7]

-- these two give the same values, but they are philosophically different. Note that ($ 2) has the type
-- (a -> b) -> b , as such, it's a "suspended computation". Here, the a -> b argument is the continuation, and
-- it specifies how the computation will be brought to a conclusion.

-- converting from a value to a continuation is easy-- use flip ($)

foo :: Int -> Int -> Int
foo x y = x + y

bar :: Int -> Int
bar x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = foo (bar x) (bar y)

-- in CPS style

foo_cps :: Int -> Int -> ((Int -> r) -> r)
foo_cps x y = \z -> z (foo x y)

bar_cps :: Int -> ((Int -> r) -> r)
bar_cps x = \z -> z (bar x)

pythagoras_cps :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps x y = \z ->
    bar_cps x $ \x_squared -> 
    bar_cps y $ \y_squared ->
    foo_cps x_squared y_squared $ z

-- this can be rather cumbersome to compose. Thankfully, continuation forms a monad!

newtype Cont r a = Cont { runCont :: (a -> r) -> r } 

instance Monad (Cont r) where
    return x = Cont ($ x)
    -- (>>=) :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
    x >>= f = Cont $ \z -> (runCont x) $ \a -> (runCont (f a)) $ z

-- now our CPS code becomes cleaner

foo_cont :: Int -> Int -> Cont r Int
foo_cont x y = return (foo x y)

bar_cont :: Int -> Cont r Int
bar_cont x = return (bar x)

pythagoras_cont :: Int -> Int -> Cont r Int
pythagoras_cont x y = do
    x_squared <- bar_cont x
    y_squared <- bar_cont y
    foo_cont x_squared y_squared


