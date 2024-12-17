import Parser

main = do input <- readFile "Day6Input.txt"
          --print $ part1 $ lines input
          print $ part2 $ lines input


type Direction = Char
type Pos = (Int,Int)
instance Num Pos where
    (x1,y1) + (x2,y2) = (x1+x2, y1+y2)

set_x (x,y) x' = (x',y) 
set_y (x,y) y' = (x,y')

type ObstacleMap = [(Pos,[Bool])]

update_obstacle :: Bool -> Pos -> Direction -> ObstacleMap -> ObstacleMap  
update_obstacle _ _ _ [] = []
update_obstacle r pos@(x,y) dir ((p,b):ps) | pos == p  = (p, b') : ps
                                           | otherwise = (p, b)  : update_obstacle r pos dir ps
    where b' = if dir == 'U' then          [r] ++ tail b      else
               if dir == 'R' then  [head b, r] ++ (drop 2 b)  else
               if dir == 'L' then   (take 2 b) ++ [r, last b] else (take 3 b) ++ [r]

is_visited :: Pos -> Direction -> ObstacleMap -> Bool
is_visited _ _ [] = False
is_visited pos@(x,y) dir ((p,b):ps) = if p == pos then b !! n else is_visited pos dir ps
    where n = if dir == 'U' then 0 else if dir == 'R' then 1 else if dir == 'L' then 2 else 3


distinct_pos :: [Pos] -> Pos -> Direction -> ObstacleMap -> Pos -> [Pos]
distinct_pos prev pos@(x,y) dir obs max@(x_max, y_max) 
    | out_of_bounds pos max = prev
    | otherwise             = distinct_pos prev' (fst new_result) (snd new_result) obs' max 
    where new_result = move pos dir (map fst obs)
          prev' = if pos `elem` prev then prev else (pos : prev)
          obs' = if dir == snd new_result then obs else (update_obstacle True (fst $ move pos dir []) dir obs)

move :: Pos -> Direction -> [Pos] -> (Pos, Direction)
move pos@(x,y) dir obs = let mod = direction_mod dir 
                          in if (pos + mod) `elem` obs then (pos, turn dir) else (pos + mod, dir)
    where turn :: Direction -> Direction
          turn d = if d == 'U' then 'R' else if d == 'R' then 'D' else
                   if d == 'D' then 'L' else 'U'
          direction_mod :: Direction -> Pos
          direction_mod d = if d == 'U' then (0,-1) else if d == 'R' then (1,0) else
                            if d == 'D' then (0,1) else (-1,0)

out_of_bounds :: Pos -> Pos -> Bool
out_of_bounds (x,y) (x_max,y_max) = x < 0 || x > x_max || y < 0 || y > y_max


part1 :: [String] -> Int
part1 xss = length $ clean [] $ distinct_pos [] start 'U' obstacles (x_max, y_max)
    where all_obstacles = [((i,j), replicate 4 False) | i <- [0..x_max], j <- [0..y_max], (xss !! j !! i) `elem` ['#','^']]
          start = fst $ head $ filter (\((x,y),_) -> xss !! y !! x == '^') all_obstacles
          obstacles = filter (\(pos@(x,y),_) -> xss !! y !! x /= '^') all_obstacles
          
          x_max = (length $ head xss) - 1 
          y_max = (length xss)        - 1

clean :: (Eq a) => [a] -> [a] -> [a]
clean _ [] = []
clean ps (x:xs) | x `elem` ps = clean ps xs
                | otherwise   = x : clean (x:ps) xs

does_loop :: Pos -> Direction -> ObstacleMap -> Pos -> Bool
does_loop pos@(x,y) dir obs max 
    | out_of_bounds pos max           = False
    | predicted_pos == fst new_result = does_loop predicted_pos dir obs max
    | otherwise                       = if is_visited predicted_pos dir obs then True else 
                                        does_loop (if pos == fst new_result then fst new_result else pos) (snd new_result) obs' max
    where new_result    = move pos dir (map fst obs)
          predicted_pos = fst $ move pos dir []
          obs'          = update_obstacle True predicted_pos dir obs

is_checked :: Pos -> Direction -> ObstacleMap -> Bool
is_checked _ _ [] = False
is_checked pos dir ((p,bs):ps) | p == pos  = bs !! (dir_index dir)
                               | otherwise = is_checked pos dir ps

dir_index :: Direction -> Int
dir_index dir = if dir == 'U' then 0 else if dir == 'R' then 1 else if dir == 'L' then 2 else 3

looping_obstacles :: [Pos] -> Pos -> Direction -> ObstacleMap -> ObstacleMap -> Pos -> Int
looping_obstacles prev pos@(x,y) dir obs checked max
    | out_of_bounds pos max                   = length $ filter (\(x,b) -> any (==True) b) checked
    | not (predicted_pos `elem` (prev ++ map fst obs))
      || is_checked predicted_pos dir checked = looping_obstacles prev' (fst new_result) (snd new_result) obs checked max
    | predicted_pos `elem` (map fst checked)  = looping_obstacles prev' (fst new_result) (snd new_result) obs (update_obstacle (does_loop pos dir ((predicted_pos, replicate 4 False) : obs) max) predicted_pos dir checked) max
    | otherwise = let resl = does_loop pos dir ((predicted_pos, replicate 4 False) : obs) max
                      new_check = if dir == 'U' then resl : replicate 3 False             else
                                  if dir == 'R' then [False, resl] ++ (replicate 2 False) else
                                  if dir == 'L' then (replicate 2 False) ++ [False, resl] else (replicate 3 False) ++ [resl]
                   in looping_obstacles prev' (fst new_result) (snd new_result) obs ((predicted_pos, new_check) : checked) max
    where new_result = move pos dir (map fst obs)
          predicted_pos = fst $ move pos dir []
          prev' = if pos `elem` prev then prev else (pos : prev)

part2 :: [String] -> Int
part2 xss = looping_obstacles [] start 'U' obstacles [(start, replicate 4 False)] (x_max, y_max)
    where all_obstacles = [((i,j), replicate 4 False) | i <- [0..x_max], j <- [0..y_max], (xss !! j !! i) `elem` ['#','^']]
          start = fst $ head $ filter (\((x,y),_) -> xss !! y !! x == '^') all_obstacles
          obstacles = filter (\((x,y),_) -> xss !! y !! x /= '^') all_obstacles
        
          x_max = (length $ head xss) - 1 
          y_max = (length xss)        - 1