module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

{-
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f
  
  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id 
-}

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
  
instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity x) = fmap Identity (f x)
  
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a
    
instance Eq a => EqProp (Identity a) where
  (=-=) = eq

testIdentity :: IO ()
testIdentity = do
  let trigger = undefined :: Identity (Int, Int, [Int])
  quickBatch (traversable trigger)
  
--

newtype Constant a b = Constant {getConstant :: a}
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a
  
instance Foldable (Constant a) where
  foldMap _ _ = mempty
  
instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)
  
instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant a

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq    
  
testConstant :: IO ()
testConstant = do
  let trigger = undefined :: Constant String (Int, Int, [Int])
  quickBatch (traversable trigger)
  
data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap f (Yep a) = Yep (f a)
  fmap _ _ = Nada
  
instance Foldable Optional where
  foldMap f (Yep a) = f a
  foldMap _ _ = mempty
  
instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = fmap Yep (f a)
  
instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
  a <- arbitrary
  frequency [(2, return $ Yep a), (1, return Nada)]
  
instance Eq a => EqProp (Optional a) where
  (=-=) = eq
  
testOptional :: IO ()
testOptional = do
  let trigger = undefined :: Optional (Int, Int, [Int])
  quickBatch (traversable trigger)
  
--

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  
instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- Maak gebruik van de noodzakelijke applicative om de functor
-- met daarin de (Cons x)-functie te applyen op het volgende deel
-- van de recursion met daarin weer een functor Cons x of functor Nil
instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> (f x) <*> traverse f xs
  
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    ls <- arbitrary
    frequency [(1, return Nil), (5, return $ Cons a ls)]
    
instance Eq a => EqProp (List a) where
  (=-=) = eq
  
testList :: IO ()
testList = do
  let trigger = undefined :: List (Int, Int, [Int])
  quickBatch (traversable trigger)
  
--

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
  
instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c
  
instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c
  
instance (Arbitrary a, Arbitrary b, Arbitrary c) 
  => Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return $ Three a b c
      
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq
  
testThree :: IO ()
testThree = do
  let trigger = undefined :: Three String (Int, Int) (Int, Int, [Int])
  quickBatch (traversable trigger)
  
-- 

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)   
  
instance Foldable (Three' a) where
  foldMap f (Three' _ y z) = f y <> f z
  
instance Traversable (Three' a) where
  traverse f (Three' x y z) = Three' x <$> f y <*> f z
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z
  
instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq
  
testThree' :: IO ()
testThree' = do
  let trigger = undefined :: Three' Int (Int, Int, [Int])
  quickBatch (traversable trigger)
  
--

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S (na) a) = S (fmap f na) (f a)
  
instance Foldable n => Foldable (S n) where
  foldMap f (S (na) a) = foldMap f na <> f a
  
instance Traversable n => Traversable (S n) where
  traverse f (S (na) a) = S <$> traverse f na <*> f a
  
instance (Arbitrary (n a), Arbitrary a) 
  => Arbitrary (S n a) where
    arbitrary = do
      na <- arbitrary
      a <- arbitrary
      return $ S na a
  
instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq
  
testS :: IO ()
testS = do
  let trigger = undefined :: S [] (Int, Int, [Int])
  quickBatch (traversable trigger)
  
--

data Tree a = 
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a) 
  deriving (Eq, Show)
  
instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node xs a ys) = Node (fmap f xs) (f a) (fmap f ys)
  
instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node xs a ys) = foldMap f xs <> f a <> foldMap f ys
  
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b) 
instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node xs a ys) = 
    Node <$> traverse f xs <*> f a <*> traverse f ys
    
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
  a <- arbitrary
  tree <- arbitrary
  frequency [(1,return Empty),
             (1, return $ Leaf a), 
             (2, return $ Node tree a tree)]

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq
  
testTree :: IO ()
testTree = do
  let trigger = undefined :: Tree(Int, Int, [Int])
  quickBatch (traversable trigger)             
           
  