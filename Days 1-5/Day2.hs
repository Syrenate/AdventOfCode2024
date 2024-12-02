import System.IO 
import Data.Char

getInput = do inp <- readFile "Day2Input.txt"
              return $ map (\x -> map read (words x)) (lines inp)

main = do inp <- getInput
          print ("Part 1: " ++ show (part1 inp))
          print ("Part 2: " ++ show (part2 inp))

          
isSafe :: Int -> [Int] -> Int    --  i == 0: undetermined          i == 1: decreasing            i == 2: increasing
isSafe _ (x:[]) = 1
isSafe i (x:xs) | i == 1 && isDecr = isSafe i xs
                | i == 2 && isIncr = isSafe i xs
                | i == 0           = if isDecr then isSafe 1 xs else
                                     if isIncr then isSafe 2 xs else 0
                | otherwise        = 0
    where isDecr = x - head xs > 0 && x - head xs <= 3
          isIncr = x - head xs < 0 && x - head xs >= -3


part1 :: [[Int]] -> Int
part1 xs = sum $ map (isSafe 0) xs

part2 :: [[Int]] -> Int
part2 xs = sum $ map (\x -> if all (== 0) (map (isSafe 0) (perms x 0)) then 0 else 1) xs
    where perms :: [a] -> Int -> [[a]]
          perms xs i | i == length xs = []
                     | otherwise      = (take i xs ++ drop (i+1) xs) : perms xs (i+1)

