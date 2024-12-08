import Parser
import System.IO

main = do inp <- readFile "Day4Input.txt"
          print $ part1 (lines inp)
          print $ part2 (lines inp)

part1 :: [String] -> Int
part1 [] = 0
part1 xss = (sum $ map grid_eval [get_grid xss (i,j) | i <- [0..(length (head xss) - 4)], j <- [0..(length xss - 4)]]) +
            (sum [length $ filter (== "XMAS") (map (\x -> take 4 (drop i x)) xss)      | i <- [0..length (head xss) - 4]]) +
            (sum [length $ filter (== "XMAS") (map (\x -> reverse $ take 4 (drop i x)) xss)      | i <- [0..length (head xss) - 4]]) +
            (sum [length $ filter (== "XMAS") (map (\x -> reverse $ take 4 x) (transpose $ drop j xss)) | j <- [0..length xss - 4]]) +
            (sum [length $ filter (== "XMAS") (map (take 4) (transpose $ drop j xss)) | j <- [0..length xss - 4]]) 
        where get_grid :: [String] -> (Int,Int) -> [String]
              get_grid [] _ = []
              get_grid ([]:yss) _ = []
              get_grid yss (i,j) = map (\y -> take 4 $ drop i y) $ take 4 (drop j yss)

              grid_eval :: [String] -> Int
              grid_eval xss = length $ filter (== "XMAS") (get_diagonal 4 xss ++ get_diagonal 4 (reverse xss))
                  | otherwise = (yss !! i) !! i : f (i-1) yss

part2 :: [String] -> Int
part2 xss = (sum $ map grid_eval [get_grid xss (i,j) | i <- [0..(length (head xss) - 3)], j <- [0..(length xss - 3)]])
    where get_grid :: [String] -> (Int,Int) -> [String]
          get_grid [] _ = []
          get_grid ([]:yss) _ = []
          get_grid yss (i,j) = map (\y -> take 3 $ drop i y) $ take 4 (drop j yss)

          grid_eval :: [String] -> Int
          grid_eval xss = if ((eval xss) + (eval $ map reverse xss)) == 2 then 1 else 0
            
          eval :: [String] -> Int
          eval xss = if (head (get_diagonal 3 xss)) `elem` ["MAS", "SAM"] then 1 else 0



transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]    : xss) = transpose xss
transpose ((x:xs): xss) = (x : map head xss) : transpose (xs : map tail xss)

get_diagonal :: Int -> [[a]] -> [[a]]
get_diagonal size xss = [result, reverse result]
    where result = f (size - 1) xss
        f :: Int -> [[a]] -> [a]
        f i yss | i < 0     = []