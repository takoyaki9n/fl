import Control.Monad.Trans

newtype MaybeT m a = MayT {runMaybeT :: m (Maybe a)}

instance Monad m => Monad (MaybeT m) where
  return x = MayT $ return (Just x)
  MayT m >>= k = 
    MayT $ do a <- m
              case a of
                Nothing -> return Nothing
                Just v -> runMaybeT (k v)

instance MonadTrans MaybeT where
  lift m = MayT $ m >>= return . Just