module VigenereCipher where

import Data.Char

-- 'a' = 97
-- 'z' = 122
-- 'A' = 65
-- 'Z' = 90
-- '0' = 48
-- '9' = 57

testText = "MEET AT DAWN"
testKey = "ALLY"

--infiniteKey :: [Char] -> [Char]
--infiniteKey key = map toLower key ++ (infiniteKey key)

toTuple :: [Char] -> [Char] -> [(Char, Char)]
toTuple text key = go text infiniteKey []
     where 
       infiniteKey = map toLower key ++ infiniteKey
       go [] _ result = reverse result
       go (' ':xs) ys result = go xs ys ((' ','a'):result)
       go xs (' ':ys) result = go xs ys result
       go (x:xs) (y:ys) result = go xs ys ((x,y):result)
       
shift :: [(Char,Char)] -> [Char]
shift ((a,b):xs) = chr y:shift xs
  where
    y | isLower a = (ord a + ord b - 194) `mod`  26 + 97  -- lower case alphabet
      | isUpper a = (ord a + ord b - 162) `mod`  26 + 65  -- upper case alphabet
      | otherwise = ord a -- all other characters stay equal
shift [] = []

vigenere :: [Char] -> [Char] -> [Char]
vigenere text key = shift (toTuple text key)


unShift :: [(Char,Char)] -> [Char]
unShift ((a,b):xs) = chr y:unShift xs
  where
    y | isLower a = (ord a - ord b) `mod`  26 + 97  -- lower case alphabet
      | isUpper a = (ord a - ord b + 32) `mod`  26 + 65  -- upper case alphabet
      | otherwise = ord a -- all other characters stay equal
unShift [] = []

unVigenere :: [Char] -> [Char] -> [Char]
unVigenere text key = unShift (toTuple text key)
