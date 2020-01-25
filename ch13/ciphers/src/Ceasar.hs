--  basic Ceasar cipher that shifts rightward (using a rightward shift of 4 'A' becomes D), don't forget an unCeasar function
--  This Ceasar includes lowercase vs capital case vs numbers and accepts other characters (but does not shift them)

module Ceasar (ceasar, unCeasar) where

import Data.Char
import Data.Maybe
import Text.Read



-- The Ceasar and unCeasar functions

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
ceasar _ [] = [] -- base case returns an empty list for an empty string

unCeasar :: Int -> String -> String
unCeasar n (x:xs)
   | ord x >= 97 && ord x <= 122 = (toWord (-n) 97 x 26):unCeasar n xs  -- shifts lower case characters from normal alphabet
   | ord x >= 65 && ord x <= 90 = (toWord (-n) 65 x 26):unCeasar n xs   -- shifts capitals from normal alphabet
   | ord x >= 48 && ord x <= 57 = (toWord (-n) 48 x 10):unCeasar n xs   -- shifts the numbers 0-10
   | otherwise = x:unCeasar n xs                                     -- other characters are not shifted
unCeasar _ [] = [] -- base case returns an empty list for an empty string

--

ceasar' :: IO ()
ceasar' = do
  putStrLn "Please input your text: "
  text <- getLine
  putStrLn "Please input the desired shift: " 
  key <- getLine
  case readMaybe key of
    Just i -> do 
            putStrLn "You converted text is: " 
            putStrLn $ ceasar i text
    Nothing -> putStrLn "Only numbers are accepted. Your text was not converted."

unCeasar' :: IO ()
unCeasar' = do
  putStrLn "Please input your text: "
  text <- getLine
  putStrLn "Please input the shift of your text: " 
  key <- getLine
  case isJust (readMaybe key :: Maybe Int) of
    True -> do 
            putStrLn "You converted text is: " 
            putStrLn $ unCeasar (read key :: Int) text
    False -> putStrLn "Only numbers are accepted. Your text was not converted."   
   