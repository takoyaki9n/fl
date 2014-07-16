module TreeSearches where 
import SearchT
import Control.Monad
{-
tr = SOr (SOr SNone (SUnit False)) (SUnit True)

inft = SOr inft (SUnit True)
-}
dfs :: SearchT a -> Maybe a
dfs t = go [t]
  where
    go []          = Nothing
    go (SNone:x)   = go x
    go (SUnit a:x) = Just a
    go (SOr l r:x) = go (l:r:x)

bfs :: SearchT a -> Maybe a
bfs t = go [t] []
  where
    go [] []         = Nothing
    go [] y          = go (reverse y) []
    go (SNone:x) y   = go x y
    go (SUnit a:x) y = Just a
    go (SOr l r:x) y = go x (l:r:y)

bfs2 :: SearchT a -> Maybe a
bfs2 t = 
  case [x | SUnit x <- queue] of
    []  -> Nothing
    a:_ -> Just a
  where
    queue = t:runBFS 1 queue
    runBFS n ts
      | n == 0 = []
      | n > 0  =
        case ts of
          SOr l r:ts' -> l:r:runBFS (n + 1) ts'
          _:ts'       -> runBFS (n - 1) ts'
          