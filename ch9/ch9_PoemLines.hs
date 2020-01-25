module Poemlines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

--putStrLn stencences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?

myLines :: String -> [String]
myLines sentence = go sentence []
  where
     go "" list = reverse list
     go string list = go (dropWhile (== '\n') (dropWhile (/= '\n') string )) ((takeWhile (/= '\n') string):list)

-- What we want 'myLines sentences' to equal
shouldEqual = 
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]
  
-- The main function here is a small test to ensure the myLines function is written correctly
main :: IO ()
main = 
  print $ "Are they equal? "
          ++ show (myLines sentences == shouldEqual)
          
-- exercise 3 write a new function that parameterizes the character you're breaking the string argument on
myLines2 :: Char -> String -> [String]
myLines2 break sentence = go sentence []
  where
     go "" list = reverse list
     go string list = go (dropWhile (== break) (dropWhile (/= break) string )) ((takeWhile (/= break) string):list)