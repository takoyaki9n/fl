module Robo where
import Data.Map hiding (size, map, filter, foldl)
import Data.List hiding (insert)

f1 = Field (20, 10) (14 ,2) [(12,0), (12,1),(8,2),(12,2),(12,3),(8,3),(8,4),(7,5),(8,5),(9,5),(10,5)]
f2 = Field (20, 10) (14 ,2) [(13,1),(14,1),(15,1),(8,2),(9,2),(10,2),(11,2),(12,2),(13,2),(15,2),(8,3),(8,4),(8,5),(8,6)]
f3 = Field (3, 3) (2 ,2) [(2, 0), (1, 1), (0, 2)]

type Point = (Integer, Integer)

data Field = Field {size :: Point, goal :: Point, walls :: [Point]}

movable :: Field -> Point -> Bool
movable f (x, y) =  inside && not_wall
  where 
    (sx, sy) = size f
    inside = 0 <= x && x < sx && 0 <= y && y <= sy
    not_wall = not (elem (x, y) (walls f))

showField :: Field -> [Point] -> String
showField f ps = 
  let (sx, sy) = size f
      g = goal f
      ws = walls f
  in concatMap (\y -> 
                 let l = map (\x -> 
                               case (x, y) of 
                                 p | p == g    -> 'g'
                                   | elem p ps -> '*'
                                   | elem p ws -> 'x'
                                   | otherwise -> '_') [0..sx - 1]
                 in l++"\n") [0..sy - 1]

data Cell = Cell {dist_to :: Integer, dist_from :: Integer, pos :: Point, link :: Maybe Cell}
data Robot = Robot {runNext :: Field -> Point -> [Point], 
                    runHeuristic :: Field -> Point -> Integer}

plus = Robot (\f (x, y) -> 
               let ps = map (\(dx, dy) -> (x + dx, y + dy)) [(1,0),(0,1),(-1,0),(0,-1)]
               in filter (movable f) ps)
             (\f (x, y) -> 
               let (gx, gy) = goal f in (abs (gx - x) + abs (gy - y)))
king = Robot (\f (x, y) -> 
               let ps = map (\(dx, dy) -> (x + dx, y + dy)) [(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1),(0,-1),(1,-1)]
               in filter (movable f) ps)
             (\f (x, y) -> 
               let (gx, gy) = goal f in max (abs (gx - x)) (abs (gy - y)))
kinght = Robot (\f (x, y) -> 
               let ps = map (\(dx, dy) -> (x + dx, y + dy)) [(2,1),(1,2),(-1,2),(-2,1),(-2,-1),(-1,-2),(1,-2),(2,-1)]
               in filter (movable f) ps)
             (\f (x, y) -> 
               let (gx, gy) = goal f in (div (abs (gx - x) + abs (gy - y) - 1) 3) + 1)

backtrack :: Cell -> [Point]
backtrack c = go c []
  where
    go d cs = 
      case link d of
        Nothing -> (pos d):cs
        Just e -> go e ((pos d):cs)

dijkstra :: Field -> Robot -> Point -> [Point]
dijkstra f r s =
  case filter (\c -> pos c == goal f) queue of
    []  -> []
    x:_ -> backtrack x
  where
    start = Cell 0 0 s Nothing
    queue = start: go 1 (insert s start empty) queue
    go 0 _  _      = []
    go n mp (x:xs) = nexts ++ (go (n + length nexts - 1) mp' xs)
      where
        p = pos x
        d = dist_to x
        ps = filter (\q -> 
                      case Data.Map.lookup q mp of
                        Nothing -> True
                        _       -> False) (runNext r f p)
        nexts = map (\q -> Cell (d + 1) 0 q (Just x)) ps
        mp' = foldl (\m y -> insert (pos y) y m) mp nexts

a_star :: Field -> Robot -> Point -> [Point]
a_star f r s = go (insert s start empty) [start]
  where
    start = Cell 0 (runHeuristic r f s) s Nothing
    go _  []     = []
    go mp (x:xs) = 
      if pos x == goal f then
        backtrack x
      else
        go mp' (merge nexts xs)
      where
        p = pos x
        d = dist_to x
        ps = filter (\q -> 
                      case Data.Map.lookup q mp of
                        Nothing -> True
                        _       -> False) (runNext r f p)
        nexts = Data.List.sortBy (\a b -> if dist_from a < dist_from b then LT else GT) $ map (\q -> Cell (d + 1) (runHeuristic r f q) q (Just x)) ps
        mp' = foldl (\m y -> insert (pos y) y m) mp nexts
        merge [] xs = xs
        merge xs [] = xs
        merge (x:xs) (y:ys)
          | dist_to x + dist_from x < dist_to y + dist_from y = x:(merge xs (y:ys))
          | otherwise = y:(merge (x:xs) ys)
