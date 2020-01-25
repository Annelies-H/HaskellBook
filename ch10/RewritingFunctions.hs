-- myAnd returns true if all Bool in the list are True
myAnd :: [Bool] -> Bool
myAnd = foldr (\a b -> if a == False then False else b) True

myAndPF :: [Bool] -> Bool
myAndPF = foldr (&&) True

-- myOr returns True if any Bool in the list is True
myOr :: [Bool] -> Bool
myOr = foldr (||) False

myOr' :: [Bool] -> Bool
myOr' = foldr (\a b -> if a == False then False else b) False

--myAny returns True if a -> Bool applied to any of the elements in the list returns True
myAny :: (a -> Bool) -> [a] -> Bool
myAny fx xs = myOr (map fx xs)

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' fx xs = foldr (||) False (map fx xs)

myAny'' :: (a -> Bool) -> [a] -> Bool
myAny'' fx xs = foldr (\a b -> if fx a == True then True else b) False xs

-- myElem returns True if a is an element in the list
-- Write two versions of myElem. One version should use folding and the other should use any
myElem :: Eq a => a -> [a] -> Bool
myElem x xs = foldr (\a b -> if a == x then True else b) False xs

myElem2 :: Eq a => a -> ([a] -> Bool)
myElem2 n = myAny (n ==)

--myReverse reverses a list
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

--Write myMap in terms of foldr. It should have the same behavior as the built-in map.
myMap :: (a -> b) -> [a] -> [b]
myMap fx xs = foldr (\a b -> fx a : b) [] xs

myMap' :: (a -> b) -> [a] -> [b]
myMap' fx xs = foldr ((:).fx) [] xs

--Write myFilter in terms of foldr. It should have the same behavior as the built-in filter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter fx xs = foldr (\a b -> if fx a == True then a : b else b) [] xs

--squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish = foldr (++) []

--squishMap maps a function over a list and concatenates the result
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap fx xs = foldr ((++).fx) [] xs

--squishAgain flattens a list of lists into a list. This time re-use the squishMap function
squishAgain :: [[a]] -> [a]
squishAgain = squishMap (id)

--myMaximumBy takes a comparison function and a list and returns the greates element of the list
-- based on the last value that the comparision returned GT for
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy fx (x:xs) = foldl (\a b -> if fx a b == GT then a else b) x xs 

--myMinimumBy takes a comparison function and a list and returns the least element of the list
--based on the last value that teh comparison returned LT for
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy fx (x:xs) = foldl (\a b -> if fx a b == LT then a else b) x xs 