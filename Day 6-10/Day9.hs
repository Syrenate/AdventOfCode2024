import Data.Char

main = do input <- readFile "Day9Input.txt"
          print $ part1 input
        
part1 :: String -> Int
part1 xss = 
    where blocks = generate_blocks 0 xss

generate_blocks :: Int -> String -> String
generate_blocks n (x:xs) | n `mod` 2 == 0 = (replicate (digitToInt x) )