import Data.Char

--exercise 2
filterUpper :: String -> String
filterUpper = filter isUpper 

--exercise 3, two versions: using head and tail and pattern matching
firstUpper :: String -> String
firstUpper xs = (toUpper (head xs)):(tail xs)

firstUpper2 :: String -> String
firstUpper2 (x:xs) = (toUpper x):xs

--exercise 4
allUpper :: String -> String
allUpper (x:xs) = (toUpper x):allUpper xs
allUpper _ = []

--exercise 5/6
firstUpperOnly :: String -> Char
firstUpperOnly = toUpper.head