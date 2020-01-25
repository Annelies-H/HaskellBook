module QuickTests where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)

half :: Fractional a => a -> a
half x = x / 2
halfIdentity :: Double -> Double
halfIdentity = (*2).half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = 
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_,False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative :: Int -> Int -> Bool
plusCommutative x y = x + y == y + x

multiAssociative :: Int -> Int -> Int -> Bool
multiAssociative x y z = x * (y * z) == (x * y) * z
multiCommutative :: Int -> Int -> Bool
multiCommutative x y = x*y == y*x

quotrem :: Int -> Int -> Bool        
quotrem _ 0 = True
quotrem x y = (quot x y)*y + (rem x y) == x
divmod :: Int -> Int -> Bool
divmod _ 0 = True
divmod x y = (div x y)*y + (mod x y) == x     

expoAssociative :: Int -> Int -> Int -> Bool
expoAssociative x y z = x ^ (y ^z) == (x ^ y) ^z
expoCommutative :: Int -> Int -> Bool
expoCommutative x y = x^y == y^x

doubleReverse :: Eq a => [a] -> Bool
doubleReverse xs = reverse (reverse xs) == id xs

--concatfold :: Foldable t => t [a] -> Bool
--concatfold xs = foldr (++) xs == concat xs

takelength :: Int -> [Int] -> Bool
takelength n xs = length (take (abs n) xs) == (abs n)

readshow x = (read (show x)) == x
        
main :: IO ()
main = hspec $ do
  describe "1. half" $ do
    it "half 4 equals 2" $ do
      half 4 `shouldBe` 2
    it "halfIdentity x returns x" $ do
      property $ \x -> x == halfIdentity x
      
  describe "2. sort" $ do
    it "a sorted list is always sorted from low to high" $ do
      property $ \xs -> listOrdered (sort (xs :: [Int])) == True

  describe "3. addition" $ do
    it "addition is associative" $ do
      property $ \x y z -> plusAssociative 
                              (x :: Int) (y :: Int) (z :: Int)
    it "addition is commutative" $ do
      property $ \x y -> plusCommutative x y  

  describe "4. multiplication" $ do
    it "multiplication is associative" $ do
      property $ \x y z -> multiAssociative x y z
    it "multiplication is commutative " $ do
      property $ \x y -> multiCommutative x y   
  
  describe "5. division" $ do
    it "quot and rem" $ do
      property $ \x y -> quotrem x y  
    it "div and mod" $ do
      property $ \x y -> divmod x y      

  describe "6. exponentiation" $ do
    it "exponentiation is associative" $ do
      property $ \x y z -> expoAssociative x y z
    it "exponentiation is commutative" $ do
      property $ \x y -> expoCommutative x y    
   
  describe "7. reverse list" $ do
    it "the reverse of a reverse list equals the list" $ do
      property $ \xs -> doubleReverse (xs :: [Int])
      
  -- 8
  
  -- 9
 -- describe "9. foldr" $ do
--    it "foldr (++) [] == concat" $ do
 --     property $ \xs -> concatfold (xs :: [[Int]])
 
  describe "10. probably not so" $ do
     it "length (take n xs) == n" $ do
       property $ \n xs -> takelength n xs
       
  describe "11. read and show round trip" $ do
    it "roundtrip returns the input integer" $ do
      property $ \x -> readshow (x :: Int)
    it "roundtrip returns the input integer list" $ do
      property $ \x -> readshow (x :: [Int])
    it "roundtrip returns the input character" $ do
      property $ \x -> readshow (x :: Char)