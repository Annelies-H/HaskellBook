module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a,a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n-d) d (count + 1)

multiplyBy :: (Integral a) => a -> a -> a
multiplyBy x y = go x y 
  where
    go x 1 = x
    go x count = x + go x (count - 1)
    
main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4 " $ do
      2 + 2 `shouldBe` 4
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

--
