module ListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


  
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    ls <- arbitrary
    frequency [(5, return $ Cons a ls), (1, return Nil)]

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

instance Monoid (List a) where
  mempty = Nil
  mappend xs Nil = xs
  mappend Nil ys = ys
  mappend (Cons x xs) ys = Cons x (mappend xs ys)
  
instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> xs = (fmap f xs) `mappend` (fs <*> xs)
  
functions = Cons (+1) (Cons (*2) Nil)
values = Cons 1 (Cons 2 Nil)
test = functions <*> values

instance Eq a => EqProp (List a) where 
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ applicative (Cons ("b", "w", 1 :: Int) Nil)