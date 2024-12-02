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
quicksort xs = let left = quicksort $ filter (<= (head xs)) (tail xs)
                   right = quicksort $ filter (> (head xs)) (tail xs)
                in left ++ [head xs] ++ right

part1 :: [String] -> Int
part1 input = sum $ map (\(x,y) -> abs (x-y)) (zip (quicksort $ get_ints input 0) (quicksort $ get_ints input 1))

part2 :: [String] -> Int
part2 input = let ys = (get_ints input 1) 
               in sum $ map (\x -> x * (length $ filter (== x) ys)) (get_ints input 0) 