import Parser

main = do input <- readFile "Day8Input.txt"
          print $ part_code '1' $ lines input
          print $ part_code '2' $ lines input
        
part_code :: Char -> [String] -> Int
part_code part xss = length $ eval_antinodes part ((length $ head xss) - 1, length xss - 1) (generate_dict xss) []

type Pos = (Int,Int)

type Dictionary = [(Char, [Pos])]

define :: Char -> Dictionary -> Dictionary
define f ds = (f,[]) : ds

keys :: Dictionary -> [Char]
keys d = map fst d

insert :: Char -> Pos -> Dictionary -> Dictionary
insert _ _ [] = [('-',[])]
insert f pos (d@(f_d, ps):ds) | f == f_d  = (f_d, (pos : ps)) : ds
                              | otherwise = d : insert f pos ds

access :: Char -> Dictionary -> [Pos]
access _ [] = [(-1,-1)]
access f ((f_d, ps):ds) | f == f_d  = ps 
                        | otherwise = access f ds

generate_dict :: [String] -> Dictionary
generate_dict xss = scan (0,0) []
    where scan :: Pos -> Dictionary -> Dictionary
          scan (i,j) d | i < 0 || i > i_max || j < 0 || j > j_max = d
                       | (xss !! j !! i) == '.'          = scan (new_pos (i,j)) d
                       | (xss !! j !! i) `elem` (keys d) = scan (new_pos (i,j)) $ insert (xss !! j !! i) (i,j) d 
                       | otherwise                       = scan (new_pos (i,j)) $ insert (xss !! j !! i) (i,j) $ define (xss !! j !! i) d
                    
          i_max = (length $ head xss) - 1
          j_max = (length xss) - 1

          new_pos :: Pos -> Pos
          new_pos (i,j) = if i == i_max then (0,j+1) else (i+1,j)

get_pairs :: [a] -> [(a,a)]
get_pairs ps = scan (0,1) ps
    where scan :: (Int,Int) -> [a] -> [(a,a)]
          scan (i,j) ps | i > length ps - 2 = []
                        | j > length ps - 1 = scan (i+1,i+2) ps
                        | otherwise         = (ps !! i, ps !! j) : (scan (i,j+1) ps)
          max = length ps - 1

eval_antinodes :: Char -> (Int,Int) -> Dictionary -> [Pos] -> [Pos]
eval_antinodes _ bound [] prev = filter (in_bounds bound) prev
eval_antinodes part bound ((f,ps):pss) prev = eval_antinodes part bound pss (prev ++ (filter (\x -> not $ x `elem` prev) (find_antinodes part bound [] ps)))

find_antinodes :: Char -> (Int,Int) -> [Pos] -> [Pos] -> [Pos]
find_antinodes part bound prev antennas = scan [] pairs
    where pairs = get_pairs antennas

          scan :: [Pos] -> [(Pos,Pos)] -> [Pos]
          scan prev [] = prev
          scan prev ((p1@(x1,y1),p2@(x2,y2)):ps) = if part == '1' then scan (prev ++ (filter (\x -> not $ x `elem` prev) 
                                                    [(x1 - 2 * (x1 - x2), y1 - 2 * (y1 - y2)), (x2 + 2 * (x1 - x2), y2 + 2 * (y1 - y2))])) ps else
                                                    scan (prev ++ (filter (\x -> not $ x `elem` prev) ((back_tiles (x1-x2) (y1-y2) p1) ++ (front_tiles (x1-x2) (y1-y2) p2)))) ps

          front_tiles :: Int -> Int -> Pos -> [Pos]
          front_tiles dx dy (x3,y3) 
              | in_bounds bound (x3,y3) = (x3,y3) : front_tiles dx dy (x3 - 2 * dx, y3 - 2 * dy)
              | otherwise               = []

          back_tiles :: Int -> Int -> Pos -> [Pos]
          back_tiles dx dy (x3,y3) 
              | in_bounds bound (x3,y3) = (x3,y3) : back_tiles dx dy (x3 + 2 * dx, y3 + 2 * dy)
              | otherwise               = []

in_bounds :: (Int,Int) -> Pos -> Bool
in_bounds (x_max, y_max) (x,y) = all (==True) [x >= 0, x <= x_max, y >= 0, y <= y_max]