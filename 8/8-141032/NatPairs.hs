module Natpairs where

natPairs = foldr1 (\ x y -> x ++ y) (map (\n -> pairs n) [2..])
  where
    pairs n | n < 2     = []
            | otherwise = map (\m -> (m, n - m)) [1..(n - 1)]
  