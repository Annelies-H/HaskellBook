module Idempotence where

import Test.Hspec
import Test.QuickCheck
import Data.Char (toUpper)
import Data.List (sort)

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: [Char] -> [Char]
capitalizeWord = map toUpper

main :: IO ()
main = hspec $ do
  describe "idempotence" $ do
    it "capitalizeWord == twice capitalizeWord" $ do
      property $ \x -> capitalizeWord x == twice capitalizeWord x
    it "capitalizeWord == fourTimes capitalizeWord" $ do
      property $ \x -> capitalizeWord x == fourTimes capitalizeWord x
    it "sort == twice sort" $ do
      property $ \x -> sort [x :: Int] == twice sort [x :: Int]
    it "sort == fourTimes sort" $ do
      property $ \x -> sort [x :: Int] == fourTimes sort [x :: Int]    