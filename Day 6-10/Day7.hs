import Parser

main = do input <- readFile "Day7Input.txt"
          print $ part1 $ lines input
          print $ part2 $ lines input

part1 :: [String] -> Int
part1 xss = sum $ map (calibration '1') xss

part2 :: [String] -> Int
part2 xss = sum $ map (calibration '2') xss

get_nums :: String -> [Int]
get_nums [] = []
get_nums xs = let res = parse (parse_num " ") xs
               in (fst $ head res) : get_nums (snd $ head res)

parse_num :: String -> Parser Int
parse_num str = do val <- parseInt; match str; return val

calibration :: Char -> String -> Int
calibration part xs | any (== test_val) $ check_operators part test_val (tail nums) (head nums) = test_val
                    | otherwise                                                                 = 0
      where result   = parse (parse_num ": ") xs
            test_val = fst $ head result
            nums     = get_nums ((snd $ head result) ++ " ")

check_operators :: Char -> Int -> [Int] -> Int -> [Int]
check_operators _ _ [] _ = []
check_operators part n (x:xs) result
      | null xs     = [result + x, result * x] ++ (if part == '2' then [concat_nums result x] else [])
      | result >= n = []
      | otherwise   = (check_operators part n xs (result + x)) ++ (check_operators part n xs (result * x))
                      ++ (if part == '2' then check_operators part n xs (concat_nums result x) else [])

concat_nums :: Int -> Int -> Int
concat_nums x y = (10^(digit_count y 0) * x) + y
      where digit_count :: Int -> Int -> Int
            digit_count val i | val < 10^i = 0
                              | otherwise  = 1 + digit_count val (i+1)