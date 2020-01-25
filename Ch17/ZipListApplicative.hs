module ZipListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

 
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

instance Applicative List where
  pure a = let infinite = Cons a infinite in infinite
--pure a = Cons a (pure a) -- less efficient memory used 
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> Cons x xs = Cons (f x) (fs <*> xs)
  
test = 
  Cons (+9) (Cons (*2) (Cons (+8) Nil)) <*> Cons 1 (Cons 2 (Cons 3 Nil))
 
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    ls <- arbitrary
    frequency [(2, return $ Cons a ls), (1, return Nil)]
    
instance Eq a => EqProp (List a) where 
  (=-=) = eq
    
newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)  

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons a xs) = Cons a (take' (n-1) xs)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l    

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs
                
instance Applicative ZipList' where
  pure a = ZipList' (pure a)
  ZipList' xs <*> ZipList' ys = ZipList' (xs <*> ys)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    list <- arbitrary
    return $ ZipList' list

main :: IO ()
main = do
  quickBatch $ applicative (ZipList' (Cons ("b", "w", 1 :: Int) Nil))
