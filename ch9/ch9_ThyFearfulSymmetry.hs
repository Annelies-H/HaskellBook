--
--Exercises: Thy Fearful Symmetry
--

-- 1

--takes the first word, pointfree
takeWord :: String -> String
takeWord = takeWhile (/= ' ') 

--drops the first word and the space behind it, pointfree
dropWord :: String -> String
dropWord = dropWhile (== ' ') . dropWhile (/= ' ')

--combine the two above
myWords :: String -> [String]
myWords sentence = go sentence []
  where 
     go "" list = reverse list
     go string list = go (dropWord string) ((takeWord string):list)
     
--now in one function
myWords2 :: String -> [String]
myWords2 sentence = go sentence []
  where
     go "" list = reverse list
     go string list = go (dropWhile (== ' ') (dropWhile (/= ' ') string )) ((takeWhile (/= ' ') string):list)