module Repmin where

data T a = Fork (T a) (T a)
         | Tip a
         deriving Show

repmin (Tip n) m = (Tip m, n)
repmin (Fork tl tr) m = (Fork tl' tr', min ml mr)
  where
    (tl', ml) = repmin tl m
    (tr', mr) = repmin tr m

run t = t'
  where
    p  = repmin t m
    t' = fst p    
    m  = snd p
