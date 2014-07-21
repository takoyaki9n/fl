module TreeSearches where 
import SearchT
import Control.Monad
{-
tr = SOr (SOr SNone (SUnit 1)) (SUnit 2)

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

iddfs :: SearchT a -> Maybe a
iddfs t = 
  case concatMap (go [(t, 0)]) [1..] of
    [] -> Nothing
    a: _ -> Just a
  where
    go [] limit = []
    go ((SNone, _): x) limit = go x limit
    go ((SUnit a, _): x) limit = [a]
    go ((SOr l r, n): x) limit = 
      if n < limit then
        go ((l, n + 1):(r, n + 1):x) limit
      else
        go x limit