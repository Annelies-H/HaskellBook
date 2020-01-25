module FunctorInstances where

import Test.QuickCheck
import Test.QuickCheck.Function

-- fmap :: (a->b) -> f a -> f b

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

--functorCompose :: (Eq (f c), Functor f) =>
--                     (a -> b)
--                  -> (b -> c)
--                  -> f a
--                  -> Bool
--functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)                  
               

functorCompose' :: (Eq (f c), Functor f) =>
                      f a
                   -> Fun a b
                   -> Fun b c
                   -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = 
  (fmap (g . f) x) == (fmap g . fmap f $ x)

 
type IntToInt = Fun Int Int
-- type IntFC = [Int] -> IntToInt -> IntToInt -> Bool
type FC = IntToInt -> IntToInt -> Bool                              
type IdentityFC = Identity Int -> FC
type PairFC = Pair Int -> FC
type TwoFC = Two String Int -> FC
type ThreeFC = Three Int Int Int -> FC
type ThreeFC' = Three' Int Int -> FC
type FourFC = Four Int Int Int Int -> FC
type FourFC' = Four' Int Int -> FC

--
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
  
identityCheck1 = quickCheck (functorCompose' :: IdentityFC)
identityCheck2 = 
  quickCheck (functorIdentity :: Identity Int -> Bool)

--
  
data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return $ Pair a1 a2
  
instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)
  
pairCheck1 = quickCheck(functorCompose' :: PairFC)
pairCheck2 = 
  quickCheck (functorIdentity :: Pair Int -> Bool)

--

data Two a b = Two a b deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b
    
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
  
twoCheck1 = quickCheck (functorCompose' :: TwoFC)
twoCheck2 = 
  quickCheck (functorIdentity :: Two String Int -> Bool)
  
  --
  
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
  
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c
    
threeCheck1 = quickCheck (functorCompose' :: ThreeFC)
threeCheck2 =
  quickCheck (functorIdentity :: Three Int Int Int -> Bool)

--

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Three' a b b

threeCheck1' = quickCheck (functorCompose' :: ThreeFC')
threeCheck2' = quickCheck (functorIdentity :: Three' Int Int -> Bool)

--

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)
  
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) 
  => Arbitrary (Four a b c d) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return $ Four a b c d
      
fourCheck1 = quickCheck (functorCompose' :: FourFC)
fourCheck2 =
  quickCheck (functorIdentity :: Four Int Int Int Int -> Bool)
  
--

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Four' a a a b
    
fourCheck1' = quickCheck (functorCompose' :: FourFC')
fourCheck2' = 
  quickCheck (functorIdentity :: Four' Int Int -> Bool)