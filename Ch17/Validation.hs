module Validation where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Validation e a = Failures e | Successes a deriving (Eq, Show)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    frequency [(1, return $ Failures e), (1, return $ Successes a)]

instance Functor (Validation e) where
  fmap _ (Failures e) = Failures e
  fmap f (Successes a) = Successes (f a)
  
instance Monoid e => Applicative (Validation e) where
  pure a = Successes a
  Successes f <*> Successes a = Successes (f a)
  Failures x <*> Failures y = Failures (mappend x y)
  _ <*> Failures e = Failures e
  Failures e <*> _ = Failures e
  
instance (Eq e, Eq a) => EqProp (Validation e a) where 
  (=-=) = eq
 
main :: IO ()
main = do
  quickBatch $ applicative (Successes ("b", "w", 1 ) :: Validation String (String, String, Int))