--
--Exercise: EnumFromTo
--
-- write your own enumFromTo defenitions for the types provided
-- should provide an empty list when the first comes after the second, e.g. enumFromTo 5 2 = []

--Remember about Bool: data Bool = False | True
--So false comes first
eftBool :: Bool -> Bool -> [Bool]
eftBool True True = [True]
eftBool False False = [False]
eftBool True False = []
eftBool False True = [False, True]

--first attempt, before realising there was something like the pred function
--the function needs reverse because otherwise it will give the intended list in reverse order, because it adds the lowest number first
--e.g. without reverse eftInt 2 4 = 4:3:2:[] = [4,3,2]
eftInt2 :: Int -> Int -> [Int]
eftInt2 start end = go start []
  where go x list
         | start > end = []
         | x == end = reverse (x:list)
         | otherwise = go (succ x) (x:list)

--actually, the start is the end and the end is the start (see eftInt2)
eftInt :: Int -> Int -> [Int]
eftInt start end = go end []
  where go y list
         | start > end = []
         | y == start = y:list
         | otherwise = go (pred y) (y:list)

--for characters it works the same as for integers
eftChar :: Char -> Char -> [Char]
eftChar start end = go end []
  where go y list
         | start > end = []
         | y == start = y:list
         | otherwise = go (pred y) (y:list)
         
--To remember about ordering: data Ordering = LT | EQ | GT
--Only specified cases where start == end or start < end, if end < start, all other cases (that is when end < start) will return and empty list
eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT LT = [LT]
eftOrd EQ EQ = [EQ]
eftOrd GT GT = [GT]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ GT = [EQ, GT]
eftOrd _  _  = []

