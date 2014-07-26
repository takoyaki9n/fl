select x xs | ys ++ [x] ++ zs =:= xs = ys ++ zs
  where ys, zs free

data Graph a = Graph {nodes :: [a], edges :: [(a, [a])]}

perm [] = []
perm xs | select x xs =:= ys = x:(perm ys)
  where x, ys free

move_from u ((v, vs):es) | (u =:= v & select w vs =:= _) ?
                           (u =/= v & move_from u es =:= w) = w
  where w free

path g [v] = select v (nodes g) =:= _ 
path g (v:u:p) = move_from u (edges g) =:= v & path g (u:p)

hamilton_path g | perm (nodes g) =:= p &
                  path g p = reverse p
  where p free
