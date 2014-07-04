import Control.Monad.Error

newtype ErrT e m a = ErrT {runErrT :: m (Either e a)}

instance (Error e, Monad m) => Monad (ErrT e m) where
  return x = ErrT $ return (Right x)
  e >>= f =
    ErrT $ do a <- runErrT e
              case a of
                Left x -> return a
                Right x -> runErrT (f x)