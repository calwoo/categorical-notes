{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FlexibleInstances #-}
import Prelude hiding (abs)

_EXERCISE_ = undefined

-----------------------------------------------------------------------------
-- Warmup: Hughes lists
-----------------------------------------------------------------------------

-- Experienced Haskellers should feel free to skip this section.

-- We first consider the problem of left-associative list append.  In
-- order to see the difficulty, we will hand-evaluate a lazy language.
-- For the sake of being as mechanical as possible, here are the
-- operational semantics, where e1, e2 are expressions and x is a
-- variable, and e1[e2/x] is replace all instances of x in e1 with e2.
--
--        e1 ==> e1'
--   ---------------------
--     e1 e2 ==> e1' e2
--
--   (\x -> e1[x]) e2 ==> e1[e2/x]
--
-- For reference, the definition of append is as follows:
--
--      a ++ b = foldr (:) b a
--
-- Assume that, on forcing a saturated foldr, its third argument is
-- forced, as follows:
--
--                e1 ==> e1'
--    -----------------------------------
--      foldr f e2 e1 ==> foldr f e2 e1'
--
--  foldr f e2 (x:xs) ==> f x (foldr f e2 xs)
--
-- Hand evaluate this implementation by forcing the head constructor,
-- assuming 'as' is not null:

listsample as bs cs = (as ++ bs) ++ cs

-- Solution:
--
--        (as ++ bs) ++ cs
--      = foldr (:) cs (as ++ bs)
--      = foldr (:) cs (foldr (:) bs as)
--      = foldr (:) cs (foldr (:) bs (a:as'))
--      = foldr (:) cs (a : foldr (:) b as')
--      = a : foldr (:) cs (foldr (:) bs as')
--
-- Convince yourself that this takes linear time per append, and that
-- processing each element of the resulting tail of the list will also
-- take linear time.

-- We now present Hughes lists:

type Hughes a = [a] -> [a]

listrep :: Hughes a -> [a]
listrep = ($ [])

append :: Hughes a -> Hughes a -> Hughes a
append f g = f . g

-- Now, hand evaluate your implementation on this sample, assuming all
-- arguments are saturated.

listsample' a b c = listrep (append (append a b) c)

-- Solution:
--
--        listrep (append (append a b) c)
--      = (\l -> l []) (append (append a b) c)
--      = (append (append a b) c) []
--      = (\z -> (append a b) (c z)) []
--      = (append a b) (c [])
--      = (\z -> a (b z)) (c [])
--      = a (b (c []))
--
-- Convince yourself that the result requires only constant time per
-- element, assuming a, b and c are of the form (\z -> a1:a2:...:z).
-- Notice the left-associativity has been converted into
-- right-associative function application.

-- The codensity transformation operates on similar principles.  This
-- ends the warmup.

-----------------------------------------------------------------------------
-- Case for leafy trees
-----------------------------------------------------------------------------

-- Some simple definitions of trees

data Tree a = Leaf a | Node (Tree a) (Tree a)

-- Here is the obvious monad definition for trees, where each leaf
-- is substituted with a new tree.

instance Monad Tree where
    return = Leaf
    Leaf x >>= f = f x
    Node l r >>= f = Node (l >>= f) (r >>= f)

instance Applicative Tree where
    pure = return
    f <*> x = do
        ff <- f
        xx <- x
        return (ff xx)

instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

-- You should convince yourself of the performance problem with this
-- code by considering what happens if you force it to normal form.

sample = (Leaf 0 >>= f) >>= f
    where f n = Node (Leaf (n + 1)) (Leaf (n + 1))

-- Let's fix this problem.  Now abstract over the /leaves/ of the tree

newtype CTree a = CTree { unCTree :: forall r. (a -> Tree r) -> Tree r }

-- Please write functions which witness the isomorphism between the
-- abstract and concrete versions of trees.

treerep :: Tree a -> CTree a
treerep (Node v) = CTree $ \r -> r v
treerep (Node l r) = 