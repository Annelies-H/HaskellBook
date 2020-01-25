module ValidatTheWord where

import Data.Char

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"
abc = "abcdefghijklmnopqrstuvwxyz"

--
-- Use the Maybe type to write a function that counts the number of vowels in a string and the number of consonants.
-- If the numbers of vowels exceeds the number of consonants, the function returns Nothing
-- In many human languages, vowels rarely exceed the number of consonants 
-- so when they do, it indicates the input isn't a real word (that is, a valid input to your dataset)

mkWord :: String -> Maybe Word'
mkWord word 
  | lengthWord > lengthABC = Nothing
  | lengthConsonants < lengthVowels = Nothing
  | otherwise = Just (Word' lowercaseWord)
  where
     lengthWord = length word
     lowercaseWord = map toLower word
     lengthABC = length (filter (\x -> elem x abc) lowercaseWord)
     lengthConsonants = lengthWord - lengthVowels
     lengthVowels = length (filter (\x -> elem x vowels) lowercaseWord)
     
-- Note:
-- I added a line to also exclude words containing characters that are not part of the alphabet
-- it will give false negatives (e.g. for "haai", the Dutch word for shark)
-- the original only accepts lower case letters, I changed it so that all words are converted to lower case