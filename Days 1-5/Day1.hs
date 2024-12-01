import System.IO 
import Data.Char

getInput = do inp <- readFile "Day1Input.txt"
              return $ lines inp

main = do inp <- getInput
          print $ part1 inp
          print $ part2 inp

get_ints :: [String] -> Int -> [Int]
get_ints [] _ = []
get_ints (x:xs) i | i == 0 = read (take 5 x) : (get_ints xs i)
                  | i == 1 = read (drop 8 x) : (get_ints xs i)          

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:[]) = [x]
quicksort xs = let xs' = tail xs
                   left = quicksort $ filter (<= (head xs)) xs'
                   right = quicksort $ filter (> (head xs)) xs'
                in left ++ [head xs] ++ right

part1 :: [String] -> Int
part1 input = sum $ map (\(x,y) -> abs (x-y)) (zip (quicksort $ get_ints input 0) (quicksort $ get_ints input 1))

occourances :: Int -> [Int] -> Int
occourances _ [] = 0
occourances n (x:xs) | x == n    = 1 + occourances n xs
                     | otherwise = occourances n xs

similarity_score :: [Int] -> [Int] -> Int
similarity_score [] _ = 0
similarity_score (x:xs) ys = (x * (occourances x ys)) + similarity_score xs ys

part2 :: [String] -> Int
part2 input = similarity_score (get_ints input 0) (get_ints input 1)