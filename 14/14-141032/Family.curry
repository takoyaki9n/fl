data Person = Namihei | Masuo | Katsuo | Tarao | Fune | Sazae | Wakame

male = Namihei
male = Masuo
male = Katsuo
male = Tarao
female = Fune
female = Sazae
female = Wakame
parent Sazae = Namihei
parent Katsuo = Namihei
parent Wakame = Namihei
parent Sazae = Fune
parent Katsuo = Fune
parent Wakame = Fune
parent Tarao = Masuo
parent Tarao = Sazae

sibling x | parent x =:= y & parent z =:= y & x =/= z = z 
  where y, z free

bloodrelative x | ancestor y =:= x ? 
                  ancestor x =:= y ? 
                  (ancestor x =:= z & ancestor y =:= z & x =/= y) = y
  where y, z free

ancestor x | parent x =:= y ? 
             (parent x =:= z & ancestor z =:= y) = y
  where y, z free

