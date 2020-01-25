module StringProcessing where

import Data.Char

-- 1.
-- Write a recursive function named replaceThe which takes a text/string,
-- breaks it into words and replaces each instance of the exact word "the" with "a".
-- notThe is a suggested helper function for accomplishing this.

notThe :: String -> Maybe String
notThe "the"  = Nothing
notThe noThe = Just noThe

replaceThe :: String -> String
replaceThe string = unwords replacedList
  where
    replacedList = replaceThe' maybeList
    maybeList = map notThe list
    list = words string
    
replaceThe' :: [Maybe String] -> [String]
replaceThe' [] = []
replaceThe' (Nothing:xs) = "a" : replaceThe' xs
replaceThe' (Just word : xs) = word : replaceThe' xs

-- 2.
-- Write a rcursive function that takes a text/string, breaks it into words, 
-- and counts the number of instances of "the" followed by a vowel-initial word

vowels :: [Char]
vowels = "aeiouAEIOU"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel string = go maybeList 0
  where
    maybeList :: [Maybe String]
    maybeList = map notThe (words string)
    
    go :: [Maybe String] -> Integer -> Integer
    go [] result = result
    go (Just word : xs) result = go xs result
    go (Nothing:Just word:xs) result 
      | elem (head word) vowels = go xs (result + 1)
      | otherwise = go xs result
    go (Nothing:Nothing:xs) result = go (Nothing:xs) result
    
-- 3.
-- Return the number of letters that are vowels in a word. Add any helper functions necessary to achieve your objectives
-- a) Test for vowelhood
-- b) Return the vowels of a String
-- c) Count the number of elements returned

countVowels :: String -> Integer
countVowels string = toInteger (length onlyVowels)
  where
    onlyVowels = filter (\x -> elem x vowels) string