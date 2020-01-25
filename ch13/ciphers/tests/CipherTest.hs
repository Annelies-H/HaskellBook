module CipherTest where

import Test.Hspec
import Test.QuickCheck
import Ceasar
import Vigenere

main :: IO ()
main = hspec $ do
  describe "Ceasar" $ do
    it "unCeasar (ceasar xs) is always equal to x" $ do
      property $ \xs -> unCeasar (ceasar xs)