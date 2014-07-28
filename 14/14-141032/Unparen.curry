data Paren = PSeq Paren Paren | PEmp

unparen "" = PEmp
unparen s  | "(" ++ s1 ++ ")" ++ s2 =:= s = PSeq (unparen s1) (unparen s2)
  where s1, s2 free

paren PEmp = ""
paren (PSeq p1 p2) = "(" ++ paren p1 ++ ")" ++ paren p2