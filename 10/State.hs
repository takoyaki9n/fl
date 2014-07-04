module State where

type ID = Int
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

newtype State s a = State (s -> (a, s))
instance Monad (State s) where
  return x = State $ \i -> (x, i)
  State s >>= f = 
    State (\i ->
            let (x, j)  = s i
                State t = f x 
            in t j)

newID = State $ \i -> (i, i + 1)

assignID :: Tree a -> State ID (Tree (a, ID))
assignID (Leaf x) = 
  do id <- newID
     return $ Leaf (x, 3)
assignID (Node l r) = 
  do l' <- assignID l 
     r' <- assignID r
     return $ Node l' r'
