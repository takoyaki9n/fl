module SearchT where 
import Control.Monad
  
data SearchT a = SNone 
               | SUnit a
               | SOr (SearchT a) (SearchT a)
               deriving Show

instance Monad SearchT where
  return = SUnit
  SNone >>= _ = SNone
  SUnit a >>= f = f a
  SOr l r >>= f = SOr (l >>= f) (r >>= f)
  fail _ = SNone

instance MonadPlus SearchT where
  mzero = SNone
  mplus = SOr
