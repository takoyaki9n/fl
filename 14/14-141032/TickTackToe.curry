import List

data Mark = O | X | E
show_mark O = "O"
show_mark X = "X"
show_mark Z = "_"
show_board b = concatMap (\r -> (concatMap show_mark r) ++ "\n") b
init3x3 = [E,E,E,E,E,E,E,E,E]

opposite X = O
opposite O = X

win_game b | p =/= E &
             ([p,p,p,_,_,_,_,_,_] =:= b ?
              [_,_,_,p,p,p,_,_,_] =:= b ?
              [_,_,_,_,_,_,p,p,p] =:= b ?
              [p,_,_,p,_,_,p,_,_] =:= b ?
              [_,p,_,_,p,_,_,p,_] =:= b ?
              [_,_,p,_,_,p,_,_,p] =:= b ?
              [p,_,_,_,p,_,_,_,p] =:= b ?
              [_,_,p,_,p,_,p,_,_] =:= b) = p
  where p free

takeone (x:xs) = x ? takeone xs

move b p | E:b' =:= b = (p:b')
  where b' free
move b p | n:b' =:= b & move b' p =:= b'' = n:b''
  where n, b', b'' free

win b p = q =:= opposite p & 
          once (\_ -> win_game b =:= p & 
                      solveAll (\_ -> win_game b =:= q) =:= []) Nothing
  where q free
win b p = q =:= opposite p & 
          b' =:= move b p & 
          lose b' q
  where q, b' free

lose b p = q =:= opposite p & 
           once (\_ -> win_game b =:= q & 
                       solveAll (\_ -> win_game b =:= p) =:= []) Nothing
  where q free
lose b p = q =:= opposite p & 
           elem E b =:= True & 
           solveAll (\b' -> move b p =:= b' & 
                            (lose b' q ? tie b' q)) =:= []
  where q free

tie b p = once (\_ -> solveAll (\_ -> win b p ? lose b p) =:= []) Nothing
