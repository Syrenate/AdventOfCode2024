module Parser(Parser,apply,parse,char,spot,
token,match,star,plus,parseInt) where

import Control.Applicative ( Alternative(some, empty, (<|>)) )
import Control.Monad
import Data.Char

newtype Parser a = P (String -> [(a,String)])

apply :: Parser a -> String -> [(a,String)] -- Applies parser to string
apply (P p) = p

parse :: Parser a -> String -> [(a,String)]
parse p s = one $ apply p s
   where one []     = []
         one [x]    = [x]
         one (x:xs) = []

instance Monad Parser where
   return x = P (\s -> [(x,s)])
   m >>= k  = P (\s -> [ (y, u) | 
                         (x, t) <- apply m s, 
                         (y, u) <- apply (k x) t ])

instance Functor Parser where
   fmap = liftM
   
instance Applicative Parser where
   pure    = return
   (<*>)   = ap

instance MonadPlus Parser where
     mzero        =  P (\s -> [])
     m `mplus` n  =  P (\s -> apply m s ++ apply n s)

instance Alternative Parser where
   empty  =  mzero
   (<|>)  =  mplus 
 

char :: Parser Char -- Parses first char of string
char = P f
   where f [] = []
         f (x:xs) = [(x,xs)]

spot :: (Char -> Bool) -> Parser Char
spot p  =  do { c <- char; guard (p c); return c }

token :: Char -> Parser Char
token c = spot (== c)

match :: String -> Parser String
match str = sequence $ map token str

-- match zero or more occurrences
star :: Parser a -> Parser [a]
star p  =  plus p `mplus` return []

-- match one or more occurrences
plus :: Parser a -> Parser [a]
plus p  =  do x <- p
              xs <- star p
              return (x:xs)

-- match a natural number
parseNat :: Parser Int
parseNat =  do s <- plus (spot isDigit)
               return (read s)

-- match a negative number
parseNeg :: Parser Int
parseNeg =  do token '-'
               n <- parseNat
               return (-n)

-- match an integer
parseInt :: Parser Int
parseInt =  parseNat `mplus` parseNeg