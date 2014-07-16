module NatLists where 
import SearchT

-- nat_lists = go []
--   where 
--     go 

nat_lists2 = concatMap go [0..]
  where
    go = (ns !!)
      where
         ns = map go' [0..]
         go' n = 
           if n == 0 then
              [[]]
           else
              concatMap
                 (\m -> 
                   let ms = go (n - m)
                   in map ((m - 1):) ms) [1..n]
