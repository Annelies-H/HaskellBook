import Data.Char

--Write a function that capitalizes a word
capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

--Write a function that capitalizes sentences in a paragraph. 
--Recognize when a new sentence has begun by checking for periods.
capitalizeParagraph :: String -> String
capitalizeParagraph (x:xs) = toUpper x : reverse function
   where 
     function = go xs []
     go [] result = result
     go ('.':' ':y:ys) result = go ys (toUpper y:' ':'.':result)
     go ('.':y:ys) result = go ys (toUpper y:'.':result)
     go (y:ys) result = go ys (y:result)