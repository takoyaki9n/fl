import List

insert x xs     = x:xs
insert x (y:ys) = y:insert x ys

perm []     = []
perm (x:xs) = insert x (perm xs)

isSorted []      = True
isSorted [a]     = True
isSorted (a:b:x) = a <= b && isSorted (b:x)

mysort xs | isSorted ys = ys
  where ys = perm xs

insertions xs = scanl (flip insert) [] (reverse xs)

mysort2 xs | all isSorted ys = last ys
  where ys = insertions xs
