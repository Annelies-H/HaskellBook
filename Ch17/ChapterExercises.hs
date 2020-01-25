module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative (liftA3)


-- given a type that has an instance of applicative 
-- specialize the types of the methods

pureone :: a -> [a]
pureone = pure

applyone :: [a -> b] -> [a] -> [b]
applyone = (<*>)

puretwo :: a -> IO a
puretwo = pure

applytwo :: IO (a -> b) -> IO a -> IO b
applytwo = (<*>)

purethree :: Monoid a => a -> (a, a)
purethree = pure

applythree :: Monoid a => (a, a -> b) -> (a,a) -> (a,b)
applythree = (<*>)

purefour :: a -> (e -> a)
purefour = pure

applyfour :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
applyfour = (<*>)

-- write applicative instances for the following datatypes

(<>) :: Monoid a => a -> a -> a
x <> y = mappend x y

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure a = Pair a a
  Pair f g <*> Pair x y = Pair (f x) (g y)
  
instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return $ Pair a a
    
instance Eq a => EqProp (Pair a) where 
  (=-=) = eq

testPair :: IO ()
testPair = do
  quickBatch $ applicative (Pair ("b", "w", 1 :: Int ) ("b", "w", 1 :: Int ))

--

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
  
instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  Two a f <*> Two b x = Two (a <> b) (f x)
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b
    
instance (Eq a, Eq b) => EqProp (Two a b) where 
  (=-=) = eq
  
testTwo :: IO ()
testTwo = do
  quickBatch $ applicative (Two "blah" (("b", "w", 1 :: Int )))
  
--

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
  
instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  Three a b f <*> Three c d x = Three (a <> c) (b <> d) (f x)
  
instance (Arbitrary a, Arbitrary b, Arbitrary c) 
  => Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <-arbitrary
      return $ Three a b c
      
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq
  
testThree :: IO ()
testThree = do
  quickBatch $ applicative (Three "b" ['a'] ("b", "w", 1 :: Int ))
  
--

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a x y) = Three' a (f x) (f y)
  
instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  Three' a f g <*> Three' b x y = Three' (a <> b) (f x) (g y)
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return $ Three' a b1 b2
    
instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq
  
testThree' :: IO ()
testThree' = do
  quickBatch $ applicative (Three' "blah" ("b", "w", 1 :: Int ) ("b", "w", 1 :: Int ))

--

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)
  
instance (Monoid a, Monoid b, Monoid c) 
  => Applicative (Four a b c) where
    pure d = Four mempty mempty mempty d
    Four a b c f <*> Four u v w x 
      = Four (a <> u) (b <> v) (c <> w) ( f x)
      
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
  => Arbitrary (Four a b c d) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return $ Four a b c d
 
instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq
  
testFour :: IO ()
testFour = do
  quickBatch $ applicative (Four "f" ["f"] "f" ("b", "w", 1 :: Int ))

--

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' u v w x) = Four' u v w (f x)
  
instance Monoid a => Applicative (Four' a) where
  pure b = Four' mempty mempty mempty b
  Four' a b c f <*> Four' u v w x = 
    Four' (a <> u) (b <> v) (c <> w) (f x)
    
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    b <- arbitrary
    return $ Four' a1 a2 a3 b
    
instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq
  
testFour' :: IO ()
testFour' = do
  quickBatch $ applicative (Four' "d" "d" "d" ("b", "w", 1 :: Int ))

-- Combinations

stops :: String
stops = "pbtdkg"
  
stops' :: [Char]
stops' = "pbtdkg"

vowels :: String
vowels = "aeiou"

vowels' :: [Char]
vowels' = "aeiou"

numbers :: [Int]
numbers = [1,2,3,4,5]

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

      