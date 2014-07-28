import List

data Mark = O | X | E
show_mark O = "O"
show_mark X = "X"
show_mark Z = "_"
show_board b = concatMap (\r -> (concatMap show_mark r) ++ "\n") b
init3x3 = [[E,E,E],[E,E,E],[E,E,E]]

opposite X = O
opposite O = X

win_game b | p =/= E &
             ([[p,p,p],[_,_,_],[_,_,_]] =:= b ?
              [[_,_,_],[p,p,p],[_,_,_]] =:= b ?
              [[_,_,_],[_,_,_],[p,p,p]] =:= b ?
              [[p,_,_],[p,_,_],[p,_,_]] =:= b ?
              [[_,p,_],[_,p,_],[_,p,_]] =:= b ?
              [[_,_,p],[_,_,p],[_,_,p]] =:= b ?
              [[p,_,_],[_,p,_],[_,_,p]] =:= b ?
              [[_,_,p],[_,p,_],[p,_,_]] =:= b) = p
  where p free

move b p | (E:x):a =:= b = (p:x):a
  where x, a free
move b p | (n:x):a =:= b & move (x:a) p =:= (y:c) = (n:y):c
  where x, y, n, a, c free
move b p | [n]:a =:= b & move a p =:= c = [n]:c
  where x, n, a, c free

win b p = once (\_ -> win_game b =:= p & findall (\_ -> win_game b =:= opposite p) =:= []) Nothing
win b p = move b p =:= b' & lose b' (opposite p)
  where b' free

lose b p = once (\_ -> win_game b =:= opposite p & findall (\_ -> win_game b =:= p) =:= []) Nothing
lose b p = move b p =:= b' & findall (\_ -> let b'' free in move b p =:= b'' & (lose b'' q ? tie b'' q)) =:= []
  where q = opposite p
        b' free

tie b p = once (\_ -> findall (\_ -> win b p) =:= [] & findall (\_ -> lose b p) =:= []) Nothing
