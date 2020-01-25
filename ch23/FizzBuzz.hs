module FizzBuzz where

{-
Write a program that prints the numbers from 1 to 100. But
for multiples of three print "Fizz" instead of the number
and for the multiples of five print "Buzz". For numbers
wich are multiples of both three and five print "FizzBuzz"
-}

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"  --first multiple of 5 & 3
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

-- this works but dumps every result seperately directly 
-- after calculating         
           
firstexample :: IO ()
firstexample = 
  mapM_ (putStrLn . fizzBuzz) [1..100]
  
-- the second example is better because its collecting the data
-- initially before dumping the results to standard output
-- the pretty bad thing is that its reversing the list
           
fizzbuzzList' :: [Integer] -> [String]
fizzbuzzList' list =
  execState (mapM_ addResult' list) []
  
addResult' :: Integer -> State [String] ()
addResult' n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)
  
secondexample :: IO ()
secondexample =
  mapM_ putStrLn $ reverse $ fizzbuzzList' [1..100]
  
-- the third example avoids reversing by appending to the
-- end of the list
           
fizzbuzzList''  :: [Integer] -> [String]
fizzbuzzList'' list = 
  let dlist = execState (mapM_ addResult'' list) DL.empty
  in DL.apply dlist [] -- convert back to normal list
  
addResult'' :: Integer -> State (DL.DList String) ()
addResult'' n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)
--snoc appeends to the end, unlike cons which appends to the front
    
thirdexample :: IO ()
thirdexample =
  mapM_ putStrLn $ fizzbuzzList'' [1..100]
  
-- the fourth example cleans it up somewhat
  
fizzbuzzList''' :: [Integer] -> DL.DList String
fizzbuzzList''' list =
  execState (mapM_ addResult''' list) DL.empty
  
addResult''' :: Integer -> State (DL.DList String) ()
addResult''' n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)
  
fourthexample :: IO ()
fourthexample =
  mapM_ putStrLn $ fizzbuzzList''' [1..100]
  
-- its often better to just enumerate the list backwards
-- first I wanted to try with recursion
  
fizzbuzzFromTo' :: Integer -> Integer -> [String]
fizzbuzzFromTo' x y = go list []
  where
    list = [y, (y-1)..x]
    go [] result = result
    go (n:ns) result = go ns (fizzBuzz n : result) 
    
exercise' :: Integer -> Integer -> IO ()
exercise' x y = mapM_ putStrLn $ fizzbuzzFromTo' x y

-- the actuall exercise, fixing the reversing fizzbuzz 
-- by eliminating the need to reverse in the first place

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo x y =
  execState (mapM_ addResult [y, (y-1)..1]) []
  
addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)
  
exercise :: Integer -> Integer -> IO ()
exercise x y = 
  mapM_ putStrLn $ fizzbuzzFromTo x y