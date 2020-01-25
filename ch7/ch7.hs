-- exercices page 224 Grab Bag

--exercise 3a
addOneIfOdd'' n = case odd n of 
  True -> f n
  False -> n
  where f n = n + 1
  
--n moet nog wel achter g staan omdat je anders de functie ontoegepast returned ipv toegepast
addOneIfOdd' n = case odd n of 
  True -> g n
  False -> n
  where g = \n ->  n + 1

addOneIfOdd n = case odd n of 
  True -> n + 1
  False -> n
 
  
--exercise 3b
addFive x y = (if x > y then y else x) + 5

addFive' x y = case x > y of
  True ->  y + 5
  False -> x + 5
  
--waarom moet ik hier nog steeds de functie aanroepen met addFive'' 1 293 en kan ik deze niet vantevoren definieren?  Omdat het een functie is
--en hij naar de dichtsbijzijnde x y kijk
addFive'' = \x y -> case x > y of
  True ->  y + 5
  False -> x + 5
  
--exercise 3 c, waarom geven deze een error in prelude? ook de originele
mflip f = \x -> \y -> f y x

mflip' x y z = z y x

--PAGE @#% EXERCISES VARIETY PACK

--Exercise 2
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

--Exercise 3
nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  EQ -> 0
  
--PAGE 244/276 ARTFUL DODGY

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

--PAGE 250/282 GUARD DUTY

-- exercise 1

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >  1    = 'X'
  | y <  1    = 'N'
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100
  
-- exercise 2

pal xs
  | xs == reverse xs = True
  | otherwise        = False
 
-- exercise 3

numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1

--CHAPTEREXERCISES p261/293

--Lets write code

--exercise 1
tensDigit :: Integral a => a -> a 
tensDigit x = result 
  where xLast = x `div` 10
        (_, result) = divMod xLast  10

        -- exercices page 224 Grab Bag

hunsDigit :: Integral a => a -> a 
hunsDigit x = result 
  where xPreLast = x `div` 100
        (_, result) = divMod xPreLast  10

--exercise 2
--if then else expression
foldBool :: a -> a -> Bool -> a 
foldBool x y z = if z == True then x else y

--case expression
foldBool1 :: a -> a -> Bool -> a 
foldBool1 x y z= case z == True of 
  True -> x
  False -> y  

--guard expression
foldBool2 :: a -> a -> Bool -> a 
foldBool2 x y z
  | z == True  = x 
  | z == False = y
  
--pattern matching
foldBool3 :: a -> a -> Bool -> a 
foldBool3 x y True  = x
foldBool3 x y False = y

--exercise 3
g :: (a -> b) -> (a, c) -> (b, c)
g f x = (f (fst x), snd x)

g' :: (a -> b) -> (a, c) -> (b, c)
g' f (a, c) = (f a, c)
  
--CHAPTER EXERCISES
--
--reviewing currying
--
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"

--
--recursion
--

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

--Mijn versie, maar die houd natuurlijk geen rekening met n = 0    en dan oneindig doorgaat, negatieve getallen werken natuurlijk ook niet      
sumTo :: (Eq a, Num a) => a -> a
sumTo n = go n 1 0
   where go n count result
          | n == count = (count + result)
          | otherwise = go n (count + 1) (count + result)

--merijns tweede versie, die 0 als check gebruikt waardoor je maar 2 variabele nodige hebt ipv de drie die ik heb          
sumTo2 :: (Eq a, Num a) => a -> a
sumTo2 n = go n 0
  where
    go 0 result = result
    go n result = go (n-1) (n + result)

--Merijns lazy versie die met grote getallen traag is omdat ie één grote lijst met op te tellen getallen maakt en dat pas aan het einde doet    
somTot :: (Eq a, Num a) => a -> a
somTot 0 = 0
somTot n = n + somTot (n-1)

--does not deal with negatives
lazyMult :: (Integral a) => a -> a -> a
lazyMult x 0 = 0
lazyMult x y  = x + lazyMult x (y-1)

--same but in where clause
multiplyBy :: (Integral a) => a -> a -> a
multiplyBy x y = go x y 
  where
    go sum 1 = 0
    go sum count = sum + go sum (count - 1)

--including negatives    
multiplyNeg :: (Integral a) => a -> a -> a
multiplyNeg x y = go x y
  where go sum count
         | count == 0 = 0
         | (sum < 0) && (count < 0 )= (negate sum) + go (negate sum) ((negate count) - 1)
         | count < 0 = (negate sum) + go (negate sum) ((negate count) - 1)
         | otherwise = sum + go sum (count - 1)

--inclusief negatief maar met merijns idee dat van recursive subtraction voor negatieve getallen, want neg - neg = positief
multiplyNeg2 :: (Integral a) => a -> a -> a 
multiplyNeg2 x y = go x y
  where go sum count
         | count == 0 = 0
         | count < 0 = go sum (count + 1) - sum
         | otherwise = go sum (count - 1) + sum   
          
--
--Fixing dividedBy
--

data DividedResult =   Result (Integer, Integer) | DividedByZero
  deriving Show

--werkt maar geeft positieve denominator bij negateve numinator
dividedFixed :: Integer -> Integer -> DividedResult
dividedFixed num denom = go num denom 0
  where go n d count
         | d == 0 = DividedByZero
         | n < 0 && d < 0 = go (negate n - negate d) (negate d) (count + 1)
         | n < d && 0 < n = Result (count, n)
         | n < 0 = go (negate (n + d)) (negate d) (count + 1)
         | n < (negate d) = Result (negate count, n)
         | d < 0 = go (n + d) d (count + 1)
         | otherwise = go (n - d) d (count + 1)

-- je kan ook meerdere branches in de where maken voor beter overzicht (nog niet uitgewerkt)         
--dividedFixed2 :: Integer -> Integer -> DividedResult
--dividedFixed2 _ 0 = DividedByZero
--dividedFixed2 num denom
--  | denom < 0 = goNeg num denom 0
--  | otherwise = goPos num denom 0
--  where
--    goNeg n d count
--      | d < n && n < 0 = Result (count, n)
--      | otherwise =
--     
--    goPos n d count
--      | n < d && 0 < n = Result (count, n)
--      | otherwise = 

-- in de go n d count functie werken met de absolute getallen zodat deze altijd hetzelfde is
-- bij het resultaat vervolgens weer de minnetjes op de goede plaats zetten
-- DividedByZero kun je ook buiten de where clause halen omdat deze altijd meteen geld
dividedFixed3 :: Integer -> Integer -> DividedResult
dividedFixed3 num denom = go (abs num) (abs denom) 0
  where go n d count
         | denom == 0 = DividedByZero
         | n < d && num < 0 && denom > 0 = Result (negate count, negate n)
         | n < d && num > 0 && denom < 0 = Result (negate count, n)
         | n < d = Result (count, n)
         | otherwise = go (n - d) d (count + 1)

--
--McCarthy 91 function
--
mc91 :: (Num a, Ord a) => a -> a
mc91 number = go number
  where go n
         | n > 100 = n - 10
         |otherwise = go (go(n+11))

mc91' :: (Num a, Ord a) => a -> a
mc91' n  
  | n > 100 = n - 10
  | otherwise = mc91' (mc91'(n+11))
         

