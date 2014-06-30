module AddCount where
import Control.Monad.State
import Control.Monad.Trans

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

data Calc a = Err String | OK a
instance Show a => Show (Calc a) where
  show (Err s) = s
  show (OK  x) = show x
instance Monad Calc where
  return  = OK
  Err s >>= f = Err s
  OK x  >>= f = f x

newtype CalcT m a = CalcT {runCalcT :: m (Calc a)}
instance Monad m => Monad (CalcT m) where
  return x = CalcT $ return (OK x)
  ct >>= f = 
    CalcT $ do c <- runCalcT ct
               case c of
                 Err x -> return (Err x)
                 OK  x -> runCalcT (f x)                 
  fail s = CalcT $ return (Err s)
instance MonadTrans CalcT where  
  lift m = CalcT $ m >>= return . OK

cntUp = state $ \i -> (i, i + 1)

eval :: Exp -> CalcT (State Int) Value
eval (EConst v) = return v
eval (EAdd e1 e2) = 
  do v1 <- eval e1
     v2 <- eval e2
     c  <- lift cntUp 
     case (v1, v2) of
       (VInt x, VInt y) -> return $ VInt (x + y)
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
