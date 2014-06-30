{-
let ex = ELet "x" (EConst (VInt 3)) (EMul (EVar "x") (EVar "x"))in eval ex
-}
module Interpreter where

import Data.Map
import Control.Monad.Reader

data Value = VInt Int
           | VBool Bool
instance Show Value where
  show (VInt i)  = show i
  show (VBool b) = show b

type Name = String
type Env = Map Name Value

data Exp = EConst Value
         | EVar Name
         | EAdd Exp Exp
         | ESub Exp Exp
         | EMul Exp Exp
         | EDiv Exp Exp
         | EEq Exp Exp
         | ELt Exp Exp
         | EIf Exp Exp Exp
         | ELet Name Exp Exp
         deriving Show

data Evald a = Err String | OK a
instance Show a => Show (Evald a) where
  show (Err s) = s
  show (OK  x) = show x
instance Monad Evald where
  return  = OK
  Err s >>= f = Err s
  OK x  >>= f = f x

newtype Calc a = Calc {runCalc :: Reader Env (Evald a)}
instance Monad Calc where
  return x = Calc $ return (OK x)
  c >>= f = 
    Calc $ do a <- runCalc c
              case a of
                Err x -> return (Err x)
                OK  x -> runCalc (f x)                 
  fail s = Calc $ return (Err s)
instance Show a => Show (Calc a) where
  show (Calc c) = show $ (runReader c) empty 

liftToCalc m = Calc $ m >>= return . OK

eval :: Exp -> Calc Value
eval (EConst v) = return v
eval (EVar n) = 
  do mv <- liftToCalc (asks (Data.Map.lookup n))
     case mv of
       Just v -> return v
       Nothing -> fail ("undefined variable: " ++ n)
eval (EAdd e1 e2) = 
  do v1 <- eval e1
     v2 <- eval e2
     case (v1, v2) of
       (VInt x, VInt y) -> return (VInt (x + y))
       _ -> fail "add: Type unmatch"
eval (ESub e1 e2) = 
  do v1 <- eval e1
     v2 <- eval e2
     case (v1, v2) of
       (VInt x, VInt y) -> return (VInt (x - y))
       _ -> fail "sub: Type unmatch"
eval (EMul e1 e2) = 
  do v1 <- eval e1
     v2 <- eval e2
     case (v1, v2) of
       (VInt x, VInt y) -> return (VInt (x * y))
       _ -> fail "mul: Type unmatch"
eval (EDiv e1 e2) = 
  do v1 <- eval e1
     v2 <- eval e2
     case (v1, v2) of
       (VInt x, VInt y) | y /= 0 -> return (VInt (div x y))        
                        | y == 0 -> fail "div: Division by 0"
       _  -> fail "div: operands must be Int Int"
eval (EEq e1 e2) = 
  do v1 <- eval e1
     v2 <- eval e2
     case (v1, v2) of
       (VInt x,  VInt y)  -> return (VBool (x == y))
       (VBool x, VBool y) -> return (VBool (x == y))
       _ -> fail "eq: Type unmatch"
eval (ELt e1 e2) = 
  do v1 <- eval e1
     v2 <- eval e2
     case (v1, v2) of
       (VInt x, VInt y) -> return (VBool (x < y))
       _ -> fail "lt: Type unmatch"
eval (EIf e1 e2 e3) = 
  do v <- eval e1
     case v of
       VBool b -> 
         if b then
           eval e2
         else
           eval e3
       _ -> fail "if: Type unmatch"
eval (ELet n e1 e2) = 
  do v   <- eval e1
     u   <- liftToCalc (local (insert n v) (runCalc (eval e2)))
     case u of
       Err m -> fail m
       OK x -> return x