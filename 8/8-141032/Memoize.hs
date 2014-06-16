module Memoize where

memoize f = 
  let rs = map f [0..] in
  (rs !!)

mfib = memoize
       (\x -> if x <= 2 then 1 else mfib (x - 1) + mfib (x - 2))

memoize2 f i = 
  let rs = map f [0..] in
  rs !! i
  
mfib2 = memoize2
       (\x -> if x <= 2 then 1 else mfib2 (x - 1) + mfib2 (x - 2))

get (x:xs) i = if i == 0 then x else get xs (i -1)

mp f (x:xs) = (f x):(mp f xs) 

memoize3 f = get (mp f [0..])
  
mfib3 = memoize3 (\x -> if x <= 2 then 1 else mfib3 (x - 1) + mfib3 (x - 2))       

memoize4 f i = get (mp f [0..]) i

mfib4 = memoize4 (\x -> if x <= 2 then 1 else mfib4 (x - 1) + mfib4 (x - 2))

