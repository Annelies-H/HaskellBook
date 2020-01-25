module WordNumber where

import Data.List (intersperse)

--a function that turns integers from 0-9 into their corresponding english words
digiToWord :: Int -> String
digiToWord n 
   | n == 0 = "zero"
   | n == 1 = "one"
   | n == 2 = "two"
   | n == 3 = "three"
   | n == 4 = "four"
   | n == 5 = "five"
   | n == 6 = "six"
   | n == 7 = "seven"
   | n == 8 = "eight"
   | n == 9 = "nine"
   | otherwise = "otherwise"

-- function that takes the integer, separates the digits and returns a list of integers
digits :: Int -> [Int]
digits number = go number [] 
  where 
    go 0 result = result
    go n result = go (div n 10) ((mod n 10):result)

--combining the first two functions turning the number into a single string with interpersed hyphens (-) "one-two-three"
wordNumber :: Int -> String
wordNumber number = concat (intersperse "-" (map digiToWord (digits number)))

--using pointfree notation
wordNumber2 :: Int -> String
wordNumber2 = concat . intersperse "-" . map digiToWord . digits

--merijn voorbeeld hoe om hoe de head functie te omzeilen als je die zou willen gebruiken in combinatie met ++ (dus omslachtig doen ipv concat gebruiken)
mijnConcat :: [String] -> String
mijnConcat [] = []
mijnConcat (x:xs) = x ++ mijnConcat xs