import System.IO
import Parser

parseInp :: Parser Int
parseInp = do c <- match "mul("
              num1 <- parseInt
              dump <- match ","
              num2 <- parseInt
              dum2 <- match ")"
              return $ num1 * num2
        
main = do inp <- readFile "Day3Input.txt"
          print $ part1 inp
          print $ part2 inp True

part1 :: String -> Int
part1 [] = 0
part1 xs | null result = part1 $ tail xs
         | otherwise = (part1 (snd $ head result)) + (fst $ head result)
    where result = parse parseInp xs :: [(Int, String)]

parseDo :: Parser Bool
parseDo = do c <- match "do()"
             return True
        
parseDont :: Parser Bool
parseDont = do c <- match "don't()"
               return False

part2 :: String -> Bool -> Int
part2 [] mult = 0
part2 xs mult | not $ null isDo   = part2 (snd $ head isDo) (fst $ head isDo)
              | not $ null isDont = part2 (snd $ head isDont) (fst $ head isDont)
              | not $ null result = (if mult == True then (fst $ head result) else 0) + part2 (snd (head result)) mult
              | otherwise         = part2 (tail xs) mult
    where result = parse parseInp xs
          isDo = parse parseDo xs
          isDont = parse parseDont xs