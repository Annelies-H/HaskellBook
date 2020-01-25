module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad
import Control.Applicative



--

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg
  
instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg
  
instance EqProp (Nope a) where
  (=-=) = eq
  
nopeTest :: IO ()
nopeTest = do
  let trigger :: Nope (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

--

data PhhhbbtttEither b a = Left' a | Right' b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right' b) = Right' b
  fmap f (Left' a) = Left' (f a)
  
instance Monoid b => Applicative (PhhhbbtttEither b) where
  pure a = Left' a
  Left' f <*> Left' a = Left' (f a)
  Right' x <*> Right' y = Right' (mappend x y)
  Right' x <*> _ = Right' x
  _ <*> Right' x = Right' x

instance Monoid b => Monad (PhhhbbtttEither b) where
  return = pure
  Left' a >>= f = f a
  Right' b >>= _ = Right' b
  
instance (Arbitrary a, Arbitrary b, Monoid b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Left' a), (1, return $ Right' b)]
    
instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq
  
eitherTest :: IO ()
eitherTest = do
  let trigger :: PhhhbbtttEither String (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  
--

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
  
instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity a = Identity (f a)
  
instance Monad Identity where
  return = pure
  Identity a >>= f = f a
  
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a
    
instance Eq a => EqProp (Identity a) where
  (=-=) = eq

identityTest :: IO ()
identityTest = do
  let trigger :: Identity (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  
--

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  
instance Monoid (List a) where
  mempty = Nil
  mappend xs Nil = xs
  mappend Nil ys = ys
  mappend (Cons x xs) ys = Cons x (mappend xs ys)
  
instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> xs = mappend (fmap f xs) (fs <*> xs)
  
instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x xs >>= f = mappend (f x) (xs >>= f)
  
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    ls <- arbitrary
    frequency [(1, return Nil), (5, return $ Cons a ls)]
    
instance Eq a => EqProp (List a) where
  (=-=) = eq

listTest :: IO ()
listTest = do
  let trigger :: List (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  
--

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = ap (fmap f a) b

l2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2' = liftA2

a' :: Monad m => m a -> m (a -> b) -> m b
a' = flip ap

meh' :: Monad m => [a] -> (a -> m b) -> m [b]
meh' xs f = sequence $ fmap f xs

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = go $ map f xs
  where 
    go :: Monad m => [m b] -> m [b]
    go [] = return []
--    go (x:xs) = x >>= \b -> fmap (b:) (go xs)
    go (x:xs) = do
        b <- x
        bs <- go xs
        return (b:bs)
--    go (x:xs) = x >>= \b -> (go xs >>= \bs -> return (b:bs))

flipType :: Monad m => [m a] -> m [a]
flipType [] = return []
flipType (x:xs) = x >>= \x -> fmap (x:) (flipType xs)

flipType' :: Monad m => [m a] -> m [a]
flipType' xs = meh xs id