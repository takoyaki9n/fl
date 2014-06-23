module Containers where

import Instantiation

class Functor f => Container f where
  contents :: f a -> [a]
  fill :: f b -> [a] -> f a

instance Functor LList where
  fmap f Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Container LList where
  contents Nil         = []
  contents (Cons x xs) = x:(contents xs)
  fill Nil         _      = Nil
  fill _           []     = Nil
  fill (Cons x xs) (y:ys) = Cons y (fill xs ys)

instance Functor BT where
  fmap f (L x)   = L (f x)
  fmap f (B l r) = B (fmap f l) (fmap f r)

instance Container BT where
  contents (L x)   = [x]
  contents (B l r) = (contents l) ++ (contents r)
  fill t xs = fst (fill' t xs)
    where 
      fill' (L x) (y:ys) = (L y, ys)
      fill' (B l r) ys   = (B l' r', ws)
        where 
          (l', zs) = fill' l ys
          (r', ws) = fill' r zs

size :: Container f => f a -> Int
size xs = length (contents xs)

sumall :: (Num a, Container f) => f a -> a
sumall xs = sum (contents xs)

--(B (B (B (L 1) (L 2)) (L 3)) (L 4))