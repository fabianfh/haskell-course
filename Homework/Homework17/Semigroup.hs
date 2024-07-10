import           Data.Monoid
import           Data.Semigroup

--------------------------------------------------------------------------------------------
---------------------------------------- NonEmpty ------------------------------------------

-- This is the type of a NonEmpty list:
data NonEmpty a = a :| [a] deriving (Show, Eq)

l1, l2, l3 :: NonEmpty Int
l1 = 1 :| [2, 3]
l2 = 4 :| [5, 6]
l3 = 7 :| [8, 9]

l4, l5 :: NonEmpty Char
l4 = 'h' :| "ello"
l5 = 'w' :| "orld"


-- 1 :| [2, 3]       <>  4 :| [5, 6]
-- 1 :| ( 2 :| [3])  <>  4 :| [5, 6]


--- As you can see, because we alwas have to provide the first `a`, we can never have an empty list.
-- Now, define a Semigroup instance for NonEmpty:

--  TODO: Define the Semigroup instance for NonEmpty
instance Semigroup (NonEmpty a) where
    (a :|[_]) <> (b:|bs)  = a :| (b:bs)  
    (a :|as) <> bs  = a :| as <> bs  

-- Test it out

--- >>> l1 <> l2
-- 1 :| [2,3,4,5,6]

--- >>> l4 <> l5
-- 'h' :| "elloworld"

--- >>> (l1 <> l2) <> l3 == l1 <> (l2 <> l3)
-- True


-- Now, try to define a Monoid instance for NonEmpty.
-- You'll notice that it's not possible to do so because there's
-- no identity element for NonEmpty.

-- instance Monoid (NonEmpty a) where
    -- mempty = undefined

