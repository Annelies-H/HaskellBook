module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

-- lookup :: Eq a => a -> [(a,b)] -> Maybe b

--zip x and y using 3 as the lookup key -> Just 6
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key -> Just 9
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- zip x and y using 4 as the lookup key -> Nothing
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

-- have x1 make a tuple of xs and xy
x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

-- have x2 make a tuple of ys and zs
x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

--x3 takes one input and makes a tuple of the results of two 
--applications of z' from above
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z, z)
  where
    z = z' n  -- of willen ze hier een applicative

-- uncurry :: (a -> b -> c) -> (a,b) -> c

-- summed is just uncurry with addition as the first argument
summed :: Num c => (c, c) -> c
summed = uncurry (+)

-- a function that lifts a boolean function over two 
-- partially applied functions
bolt :: Integer -> Bool
bolt x = x > 3 && x < 8 -- Volgens mij willen ze iets anders/hipper
bolt' :: Integer -> Bool
bolt' = liftA2 (&&) (>3) (<8)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x,y]
  print $ sequenceA [xs,ys]
  print $ summed <$> x1
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z  
  print $ sequenceA [(>3), (<8), even] 7
    -- where f ~ (->) a and t ~ []
    -- dus hij haalt de functies uit de lijst, geeft 7 als
    -- argument en geeft een lijst met resultaten terug????
    -- hij heeft een lijst met functies, namelijk [a -> Bool]
    -- die veranderd hij in een functie van a > [Bool]
    -- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
    -- [Int -> Bool] == [] ((->) Int Bool)
    -- geeft t ~ [] en f ~ ((->) Int) en a ~ Bool
    -- resultaat dus f (t a) word ((->) Int) ([] Bool)
    -- is ((->) Int) [Bool]
    -- is Int -> [Bool]
    
    

--fromMaybe :: a -> Maybe a -> a
--sequenceA :: (Applicative f, Traversable t) -> t (f a) -> f (t a)

  
sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)  -- Just 15

main2 :: IO ()
main2 = do
  print $ foldr (&&) True (sequA 7)
  print $ foldr (&&) True (sequA 6)
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys