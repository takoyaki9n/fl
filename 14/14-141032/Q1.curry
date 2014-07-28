take xs n | ys ++ zs =:= xs & length ys =:= n = ys 
  where ys, zs free

drop xs n | ys ++ zs =:= xs & length ys =:= n = zs 
  where ys, zs free

last xs | _ ++ [x] =:= xs = x
  where x free

sillyUnparen "" = ""
sillyUnparen str | "(" ++ s ++ ")" =:= str = s where s free
sillyUnparen str | head str =/= '(' ? last str =/= ')' = str
