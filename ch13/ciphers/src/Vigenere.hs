module Vigenere (vigenere, unVigenere) where

import Data.Char

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

--
vigenere' :: IO ()
vigenere' = do
  putStrLn "Please input your text: "
  text <- getLine
  putStrLn "Please input your key: " 
  key <- getLine
  if checkKey key 
    then do 
      putStrLn "You converted text is: " 
      putStrLn $ vigenere text key
    else do
      putStrLn "The key must contain one or more words. Each containing only characters from the normal alphabet."
      putStrLn "Your text was not converted."

unVigenere' :: IO ()
unVigenere' = do
  putStrLn "Please input your text: "
  text <- getLine
  putStrLn "Please input your key: " 
  key <- getLine
  if checkKey key 
    then do 
      putStrLn "You converted text is: " 
      putStrLn $ unVigenere text key
    else do
      putStrLn "The key must contain one or more words. Each containing only characters from the normal alphabet."
      putStrLn "Your text was not converted."

acceptedKey :: String
acceptedKey = ['a'..'z'] ++ " " ++ ['A'..'Z']

checkKey :: String -> Bool
checkKey [] = False
checkKey key = go key
  where
    go [] = True
    go (x:xs) = case elem x acceptedKey of
                  True -> go xs
                  False -> False

