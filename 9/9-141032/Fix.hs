module Fix where

newtype Fix f = In (f (Fix f))

data NatF a = Zero | Succ a
            deriving Show
instance Functor NatF where
  fmap f Zero     = Zero
  fmap f (Succ x) = Succ (f x)

data ListF b a = Nil | Cons b a 
               deriving Show
instance Functor (ListF b) where
  fmap f Nil         = Nil
  fmap f (Cons x xs) = Cons x (f xs)

cata :: Functor f => (f a -> a) -> Fix f -> a
cata g  = g . fmap (cata g) . out
  where
    out (In f) = f

toList = cata f 
  where 
    f Nil         = []
    f (Cons x xs) = x:xs

toInt = cata f 
  where 
    f Zero     = 0
    f (Succ n) = 1 + n

add n = cata f
  where
    f Zero = n
    f (Succ m) = In (Succ m)

sum = cata f
  where
    f Nil         = In Zero
    f (Cons n ns) = add n ns
