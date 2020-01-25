module ChapterExercises where

import Data.Semigroup
import Test.QuickCheck hiding (Success, Failure)
import MonoidCheck

-- Trival: Semigroup and Monoid

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial
  
instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial
  
type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Identity a: Semigroup and Monoid

newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
 (Identity x) <> (Identity y) = Identity (x <> y)
 
instance (Monoid a, Semigroup a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)
 
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = 
  Identity String -> Identity String -> Identity String -> Bool    
    
-- Two: Semigroup and Monoid 

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) 
  => Semigroup (Two a b) where
    Two a b <> Two x y = Two (a <> x) (b <> y)
    
instance (Monoid a, Semigroup a, Monoid b, Semigroup b) 
  => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc =
     Two String String 
  -> Two String String 
  -> Two String String
  -> Bool

-- Three: Semigroup

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Eq a, Semigroup b, Eq b, Semigroup c, Eq c) 
  => Semigroup (Three a b c) where
    Three a b c <> Three u v w = Three (a <> u) (b <> v) (c <> w)

instance (Arbitrary a, Arbitrary b, Arbitrary c) 
 => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)
    
type ThreeAssoc =
     Three String String String 
  -> Three String String String 
  -> Three String String String
  -> Bool
  
 
    
-- Four: Semigroup

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Eq a, Semigroup b, Eq b, Semigroup c, Eq c, Semigroup d, Eq d) 
  => Semigroup (Four a b c d) where
    Four a b c d <> Four u v w x = Four (a <> u) (b <> v) (c <> w) (d <> x)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) 
 => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)
    
type FourAssoc =
     Four String String String String
  -> Four String String String String
  -> Four String String String String
  -> Bool
    
-- BoolConj: Semigroup and Monoid

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  BoolConj _    <> BoolConj _    = BoolConj False
  
instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = do
    x <- arbitrary 
    return (BoolConj x)

type BoolConjAssoc =
     BoolConj
  -> BoolConj
  -> BoolConj
  -> Bool
  
-- 7

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj True  <> BoolDisj _    = BoolDisj True
  BoolDisj False <> BoolDisj True = BoolDisj True
  BoolDisj _     <> BoolDisj _    = BoolDisj False
  
instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = do
    x <- arbitrary 
    return (BoolDisj x)

type BoolDisjAssoc =
     BoolDisj
  -> BoolDisj
  -> BoolDisj
  -> Bool
    
-- Or a b: Semigroup

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst _ <> Snd y = Snd y
  Snd x <> Snd _ = Snd x
  Snd x <> Fst _ = Snd x
  Fst _ <> Fst y = Fst y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return (Fst a)), (1, return (Snd b))]
  
type OrAssoc =
     Or String String
  -> Or String String
  -> Or String String
  -> Bool
  
-- Combine a b: Semigroup and Monoid

newtype Combine a b = Combine {unCombine :: (a -> b)} 

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ \x -> f x <> g x
  
instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
  mempty = Combine (const mempty)
  mappend = (<>)
  

-- Comp a: Semigroup and Monoid

newtype Comp a = Comp {uncomp :: (a -> a)}

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f.g)
  
instance Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)
  
-- Validation a b: Semigroup

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Success b)  <> _           = Success b
  _            <> (Success b) = Success b
  (Failure x)  <> (Failure y) = Failure (x <> y)
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return (Failure a)), (1, return (Success b))]
    
type ValidAssoc =
     Validation String String
  -> Validation String String
  -> Validation String String
  -> Bool  
  
-- AccumulateRight a b: Semigroup

newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)
  
instance Semigroup b => Semigroup (AccumulateRight a b) where
  (AccumulateRight (Failure a)) <> _ = AccumulateRight (Failure a)
  _ <> (AccumulateRight (Failure a)) = AccumulateRight (Failure a)
  (AccumulateRight (Success x)) <> (AccumulateRight (Success y)) =
    AccumulateRight (Success (x <> y))
  
instance (Arbitrary a, Arbitrary b) 
  => Arbitrary (AccumulateRight a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      frequency [(1, return (AccumulateRight (Failure a))),
                 (1, return (AccumulateRight (Success b)))]
                
type AccuRightAssoc =
     AccumulateRight String String
  -> AccumulateRight String String
  -> AccumulateRight String String
  -> Bool 

-- AccumulateBoth a b: Semigroup
newtype AccumulateBoth a b = 
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b)
  => Semigroup (AccumulateBoth a b) where
    (AccumulateBoth (Success x)) <> (AccumulateBoth (Success y)) =
      AccumulateBoth (Success (x <> y))
    (AccumulateBoth (Failure x)) <> (AccumulateBoth (Failure y)) =
      AccumulateBoth (Failure (x <> y))
    (AccumulateBoth x) <> (AccumulateBoth y) =
      AccumulateBoth (x <> y)
      
instance (Arbitrary a, Arbitrary b)
  => Arbitrary (AccumulateBoth a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      frequency [(1, return (AccumulateBoth(Failure a))),
                 (1, return (AccumulateBoth (Success b)))]
                 
type AccuBothAssoc =
     AccumulateBoth String String
  -> AccumulateBoth String String
  -> AccumulateBoth String String
  -> Bool
  
-- Mem s a: Monoid
newtype Mem s a = Mem {runMem :: s -> (a,s)}

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend (Mem f) (Mem g) =
    Mem $ \s -> let (a,s') = f s
                    (c,d) = g s'
                in (a `mappend` c, d)
{- 
het volgende  werkt niet omdat je niet kan patternmatchen 
op functies, kan wel met bovenstaande let in
  mappend (Mem $ \x -> (a, f)) (Mem $ \s -> (b, g)) =
    Mem $ \s -> (a <> b, f.g)
-}

{-
het volgende werkt wel voor het voorbeeld maar klopt niet
voor a en b gebruik je dezelfde s, maar voor de functies
gebruik je de s die van de andere functie komt dus als de 
functie ook de a veranderd afhankelijk van s krijg je een 
ander resultaat (bijvoorbeeld de functie 'even'
  mappend (Mem f) (Mem g) =
    Mem $ \s -> (fst (f s) `mappend` fst (g s), 
                 snd $ f (snd (g s)))
--}

f' :: Mem Integer [Char]  
f' = Mem $ \s -> ("Hi", s + 1)

--
-- QuickCheck
--

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  putStrLn "Trivial"
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  putStrLn "Identity a"
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: (Identity String) -> Bool)
  quickCheck (monoidRightIdentity :: (Identity String) -> Bool)
  putStrLn "Two a b"
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: (Two String String) -> Bool)
  quickCheck (monoidRightIdentity :: (Two String String) -> Bool)
  putStrLn "Three a b c"
  quickCheck (semigroupAssoc :: ThreeAssoc)
  putStrLn "Four a b c d"
  quickCheck (semigroupAssoc :: FourAssoc)
  putStrLn "BoolConj"
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  putStrLn "BoolDisj"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  putStrLn "Or a b"
  quickCheck (semigroupAssoc :: OrAssoc)
  putStrLn "Validation a b"
  quickCheck (semigroupAssoc :: ValidAssoc)
  putStrLn "AccumulateRight a b"
  quickCheck (semigroupAssoc :: AccuRightAssoc)
  putStrLn "AccumulateBoth a b"
  quickCheck (semigroupAssoc :: AccuBothAssoc)
  putStrLn "Mem s a"
  print $ runMem (f' `mappend` mempty) 0
  print $ runMem (mempty `mappend` f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' `mappend` mempty) 0 == runMem f' 0
  print $ runMem (mempty `mappend` f') 0 == runMem f' 0