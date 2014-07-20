module NatLists where 
import SearchT

nat_list_tree xs = SOr (SUnit xs) (foldr1 SOr (map (nat_list_tree . (:xs)) [0..]))

nat_lists =  [x | SUnit x <- queue]
  where
    root = nat_list_tree []
    queue = root:runBFS 1 queue
    runBFS n ts
      | n == 0 = []
      | n > 0  =
        case ts of
          SOr l r:ts' -> l:r:runBFS (n + 1) ts'
          _:ts'       -> runBFS (n - 1) ts'

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
