module ListLike where

class ListLike f where
  nil :: f a
  cons :: a -> f a -> f a
  app :: f a -> f a -> f a
  toList :: f a -> [a]
  fromList:: [a] -> f a

snoc :: ListLike f => f a -> a -> f a
snoc xs x = app xs (cons x nil)

rev :: ListLike f => [a] -> f a
rev [] = nil
rev (x:xs) = snoc (rev xs) x

data LL a = N | C a (LL a) | F (LL a) (LL a) deriving Show

instance ListLike LL where
  nil = N
  cons x xs = C x xs
  app xs ys = F xs ys
  toList N = []
  toList (C x xs) = x:(toList xs)
  toList (F xs ys) = (toList xs) ++ (toList ys)
  fromList [] = N
  fromList (x:xs) = C x (fromList xs)

reverse :: LL a -> LL a
reverse xs = rvs xs N
  where
    rvs N ys = ys
    rvs (C x xs) ys = rvs xs (C x ys)
    rvs (F xs ys) zs = rvs ys (rvs xs zs)