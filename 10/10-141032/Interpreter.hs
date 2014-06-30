module Interpreter where

data Value = VInt Int
           | VBool Bool
instance Show Value where
  show (VInt i)  = show i
  show (VBool b) = show b

data Exp = EConst Value
         | EAdd Exp Exp
         | ESub Exp Exp
         | EMul Exp Exp
         | EDiv Exp Exp
         | EEq Exp Exp
         | ELt Exp Exp
         | EIf Exp Exp Exp
         deriving Show

data Calc a = Err String | Ok a
instance Show a => Show (Calc a) where
  show (Err s) = s
  show (Ok  x) = show x

instance Monad Calc where
  return  = Ok
  Err s >>= f = Err s
  Ok x  >>= f = f x

eval :: Exp -> Calc Value
eval (EConst v) = return v
eval (EAdd e1 e2) = 
  do v1 <- eval e1
     v2 <- eval e2
     case (v1, v2) of
       (VInt x, VInt y) -> return (VInt (x + y))
       _ -> Err "add: Type unmatch"
eval (ESub e1 e2) = 
  do v1 <- eval e1
     v2 <- eval e2
     case (v1, v2) of
       (VInt x, VInt y) -> return (VInt (x - y))
       _ -> Err "sub: Type unmatch"
eval (EMul e1 e2) = 
  do v1 <- eval e1
     v2 <- eval e2
     case (v1, v2) of
       (VInt x, VInt y) -> return (VInt (x * y))
       _ -> Err "mul: Type unmatch"
eval (EDiv e1 e2) = 
  do v1 <- eval e1
     v2 <- eval e2
     case (v1, v2) of
       (VInt x, VInt y) | y /= 0 -> return (VInt (div x y))        
                        | y == 0 -> Err "div: Division by 0"
       _  -> Err "div: operands must be Int Int"
eval (EEq e1 e2) = 
  do v1 <- eval e1
     v2 <- eval e2
     case (v1, v2) of
       (VInt x,  VInt y)  -> return (VBool (x == y))
       (VBool x, VBool y) -> return (VBool (x == y))
       _ -> Err "eq: Type unmatch"
eval (ELt e1 e2) = 
  do v1 <- eval e1
     v2 <- eval e2
     case (v1, v2) of
       (VInt x, VInt y) -> return (VBool (x < y))
       _ -> Err "lt: Type unmatch"
eval (EIf e1 e2 e3) = 
  do v <- eval e1
     case v of
       VBool b -> 
         if b then
           eval e2
         else
           eval e3
       _ -> Err "if: Type unmatch"
