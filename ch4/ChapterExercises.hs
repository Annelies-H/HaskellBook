awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

-- 1
-- length :: [a] -> Int
-- length takes one argument that is a list and evualuates into a whole number, probably Int or something
-- the actual type is: length :: Foldable t => t a -> Int

--2
--length [1,2,3,4,5] = 5
--length [(1,2),(2,3),(3,4) = 3
--length allAwesome = 2
--length (concat allAwesome) = 1  --(concat takes a list of lists and returns one list with all values)

--3
thisworks = 6 / 3 
-- (/) requires a fractional number while lenght returns an intiger
--therefore 6 / length [1,2,3] does not work

--4
thisalsoworks = 6 `div` length [1,2,3]  -- 'div' accepts integrals

--5
ex5 :: Bool
ex5 = 2 + 3 == 5 -- the expected result = False

--6
x = 5
ex6 :: Bool
ex6 = x + 3 == 5 -- == True

--7
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

--8
myAbs :: Integer -> Integer
myAbs x = if x > 0 then x else x * (-1)
myAbs' x = if x > 0 then x else negate x
            
f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f x y = (,) t1 t2
      where 
       t1 = (,) (snd x) (fst y)
       t2 = (,) (fst x) (snd y)

