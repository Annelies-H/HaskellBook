--first attempt
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y):myZip xs ys

--however, no need for seperate cases because an empty list will not pattern match with (x:xs) and [1] = 1:[] so does match
myZip2 :: [a] -> [b] -> [(a, b)]
myZip2 (x:xs) (y:ys) = (x, y):myZip xs ys
myZip2 _ _ = []

--exercise 2
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = (f x y):myZipWith f xs ys
myZipWith f _ _ = []

--exercise 3 (,) creates a tuple of two values
myZip3 :: [a] -> [b] -> [(a, b)]
myZip3 x y = myZipWith (,) x y

--pointfree
myZip4 :: [a] -> [b] -> [(a, b)]
myZip4 = myZipWith (,) 

--using head and tail instead of pattern matching, turns out it requires x and y to have an isntance of Equal
myZip5 :: (Eq a, Eq b) => [a] -> [b] -> [(a, b)]
myZip5 x y = go x y
  where go x' y'
         | x' == [] = []
         | y' == [] = []
         | otherwise = ((,) (head x) (head y)):myZip5 (tail x) (tail y)
         
-- use null to avoid needing an instance of Equal, null checks if something is empty: null [] = True
myZip6 :: [a] -> [b] -> [(a, b)]
myZip6 x y = go x y
  where go x' y'
         | null x' = []
         | null y' = []
         | otherwise = ((,) (head x) (head y)):myZip6 (tail x) (tail y)
         
-- using 'case of'
myZip7 :: [a] -> [b] -> [(a,b)]
myZip7 xs ys = case null xs || null ys of
      True  -> []
      False -> ((,) (head xs) (head ys)):myZip7 (tail xs) (tail ys)
      
-- using 'case of' with pattern matching & head and tail , both lists are combined in a tuple to avoid the need of nested 'case of' expressions
myZip8 :: [a] -> [b] -> [(a,b)]
myZip8 xs ys = case (xs,ys) of
      ([],_)  -> []
      (_,[])  -> []
      (_,_) -> ((,) (head xs) (head ys)):myZip8 (tail xs) (tail ys)


-- using 'case of' with pattern matching & without head and tail
-- seems like for some reason it needs the specific matches unlike the original pattern matching myZip
myZip9 :: [a] -> [b] -> [(a,b)]
myZip9 (x:xs) (y:ys) = case (x:xs,y:ys) of
      (x:xs,[]) -> []
      ([],y:ys) -> []
      ([],[]) -> []
      (x:xs,y:ys)  -> (x, y):myZip9 xs ys




















