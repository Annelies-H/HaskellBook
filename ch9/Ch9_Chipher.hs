--  basic Ceasar cipher that shifts rightward (using a rightward shift of 4 'A' becomes D), don't forget an unCeasar function

module Cipher where

import Data.Char

--chr changes an integer to the corresponding character
--ord changes a character to the corresponding integer
--use mod to wrap the shift

-- 'a' = 97
-- 'z' = 122
-- 'A' = 65
-- 'Z' = 90
-- '0' = 48
-- '9' = 57

--
--Third attempt
--all other characters are now needlessly switched to numbers and back again (because of the chr outside of the where), but is easier to read

ceasar3 :: Int -> String -> String
ceasar3 n (x:xs) = chr y:ceasar3 n xs
  where
    y | isLower x = (ord x - 97 + n) `mod`  26 + 97  -- lower case alphabet
      | isUpper x = (ord x - 65 + n) `mod`  26 + 65  -- upper case alphabet
      | isDigit x = (ord x - 48 + n) `mod`  10 + 48  -- numbers 0-9
      | otherwise = ord x -- all other characters stay equal
ceasar3 n _ = []


unCeasar3 :: Int -> String -> String
unCeasar3 n (x:xs) = chr y:unCeasar3 n xs
  where
    y | isLower x = (ord x - 97 - n) `mod`  26 + 97
      | isUpper x = (ord x - 65 - n) `mod`  26 + 65
      | isDigit x = (ord x - 48 - n) `mod`  10 + 48
      | otherwise = ord x
unCeasar3 n _ = []

--
--SECOND attempt includes lowercase vs capital case vs numbers and accepts other characters (but does not shift them)
toNumber :: Int -> Char -> Int
toNumber z x = (ord x) - z

shift :: Int -> Int -> Char -> Int -> Int
shift n z x a = mod ((toNumber z x)+n) a

toWord :: Int -> Int -> Char -> Int -> Char
toWord n z x a = chr ((shift n z x a)+z)

ceasar :: Int -> String -> String
ceasar n (x:xs) 
   | ord x >= 97 && ord x <= 122 = (toWord n 97 x 26):ceasar n xs  -- shifts lower case characters from normal alphabet
   | ord x >= 65 && ord x <= 90 = (toWord n 65 x 26):ceasar n xs   -- shifts capitals from normal alphabet
   | ord x >= 48 && ord x <= 57 = (toWord n 48 x 10):ceasar n xs   -- shifts the numbers 0-10
   | otherwise = x:ceasar n xs                                     -- other characters are not shifted
ceasar n _ = [] -- base case returns an empty list for an empty string

--unCeasar is basically ceasar but reversing the shift by negating n  
unCeasar :: Int -> String -> String
unCeasar n (x:xs)
   | ord x >= 97 && ord x <= 122 = (toWord (-n) 97 x 26):unCeasar n xs  -- shifts lower case characters from normal alphabet
   | ord x >= 65 && ord x <= 90 = (toWord (-n) 65 x 26):unCeasar n xs   -- shifts capitals from normal alphabet
   | ord x >= 48 && ord x <= 57 = (toWord (-n) 48 x 10):unCeasar n xs   -- shifts the numbers 0-10
   | otherwise = x:unCeasar n xs                                     -- other characters are not shifted
unCeasar n _ = [] -- base case returns an empty list for an empty string

--
--FIRST ATTEMPT
--only works with single words and small letters, not with sentences or capital letters
--still needs the unCeasar, but thats 'just' reversing everything

--transform all charachters to corresponding number and subtract 97 (='a')
toNumber1 :: String -> [Int]
toNumber1 word = map (+(-97)) (map ord word)

--shift the numbers and modular devide around 26
shift1 :: Int -> String -> [Int]
shift1 n word = map (\x -> mod (x+n) 26) (toNumber1 word)

--add 97 again and change the numbers back to a word
ceasar1 :: Int -> String -> String
ceasar1 n word = map chr (map (+97) (shift1 n word))