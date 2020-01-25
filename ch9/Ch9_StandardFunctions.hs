--myOr returns True if any in the list is True
myOr :: [Bool] -> Bool
myOr (True:xs) = True
myOr (False:xs) = myOr xs
myOr [] = False

myOr' :: [Bool] -> Bool
myOr' [] = True
myOr' (x:xs) = if x == True then True else myOr' xs
--
--myAny returns True if a -> Bool applied to any of the functions in the list returns True
myAny :: (a -> Bool) -> [a] -> Bool
myAny fx [] = False
myAny fx (x:xs) = case fx x of
  True -> True
  False -> myAny fx xs  

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' fx xs = myOr (map fx xs)

--
--myElem, returns True if a is an element in the list
myElem :: Eq a => a -> [a] -> Bool
myElem y [] = False
myElem y (x:xs) = case y == x of
  True -> True
  False -> myElem y xs
  
myElem2 :: Eq a => a -> [a] -> Bool
myElem2 y list = myAny (\x -> x == y) list

--
--myReverse reverses a list
myReverse :: [a] -> [a]
myReverse (x:xs) = go (x:xs) []
  where 
    go [] result = result
    go (x:xs) result = go xs (x:result) 

--    
--squish flattens a list of lists into a lists
squish :: [[a]] -> [a]
squish [] = []
squish (xs:ys) = xs ++ squish ys

squish2 :: [[a]] -> [a]
squish2 [] = []
squish2 (x:xs) = go x xs
  where
    go :: [a] -> [[a]] -> [a]
    go [] rest = squish2 rest
    go (x:xs) rest = x:go xs rest
   
--
--squishMap maps a function over a list and concatenates the results
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap fx [] = []
squishMap fx (x:xs) = fx x ++ squishMap fx xs


--
--squishAgain flattens a list into a list, re-using the squishMap function
squishAgain :: [[a]] -> [a]
squishAgain list = squishMap id list 

squishAgain2 :: [[a]] -> [a]
squishAgain2 (x:xs) = squishMap (\x -> x) (x:xs)

--myMaxiumumBy takes a comparison fucntion and a list and returns the greatest element of the list
--based on the last value that the comparison returned GT for
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy fx (x:xs) = go fx (xs) x
  where 
     go fx [] result = result
     go fx (y:ys) result
       | fx y result == GT = go fx ys y
       | otherwise = go fx ys result
 
--myMinimumBy takes a comparison function and a list and returns the least element of the list
--based on the last value that the comparision returned LT for
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy fx (x:xs) = go xs x
  where 
     go [] result = result
     go (y:ys) result
       | fx y result == LT = go ys y
       | otherwise = go ys result
       
--Using the myMinimumBy and myMaximumBy functions write your own function of maximum and minimum
myMaximum :: (Ord a) => [a] -> a
myMaximum (x:xs) = myMaximumBy compare (x:xs)

myMinimum :: (Ord a) => [a] -> a
myMinimum (x:xs) = myMinimumBy compare (x:xs)