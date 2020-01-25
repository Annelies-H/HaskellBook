import Data.Char

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf = undefined

-- without the As-pattern
isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' list1 list2 = and recursion
     where 
       recursion = go list1 list2 []
       go []     _  result = result
       go (x:xs) ys result = go xs ys (elem x ys: result)
       
       
-- Split a sentence into words, then tuple each word with the capitalised form of each
capitalizeWords :: String -> [(String, String)]
capitalizeWords sentence = map function list
     where
       list = words sentence
       function word@(x:xs) = (word , toUpper x : xs)

