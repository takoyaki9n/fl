module Sorts where

insSort [] = []
insSort (x:xs) = 
  insert x (insSort xs)
  where 
    insert x [] = [x]
    insert x (y:ys) 
      | x < y     = (x:y:ys)
      | otherwise = y:(insert x ys)
    
quickSort [] = []
quickSort (x:xs) = (quickSort (filter (<= x) xs)) ++ [x] ++ (quickSort (filter (> x) xs))
    
selectionSort [] = []
selectionSort (x:xs) =
  m:(selectionSort r)
  where 
    (m, r) = selmin x xs
    selmin x []     = (x, [])
    selmin x (y:ys) = (fst pr, gt:(snd pr))
      where
        gt = max x y
        lt = min x y
        pr = selmin lt ys

mergeSort xs =
  msort (map (\x -> [x]) xs)
  where
    merge2 [] xs = xs
    merge2 xs [] = xs
    merge2 (x:xs) (y:ys)
      | x < y     = x:(merge2 xs (y:ys))
      | otherwise = y:(merge2 (x:xs) ys)
    mergen [] = []
    mergen [x] = [x]
    mergen (x:y:z) = (merge2 x y):(mergen z)
    msort [] = []
    msort [xs] = xs
    msort xss = msort (mergen xss)
    