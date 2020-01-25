import Test.QuickCheck 
import Test.QuickCheck.Gen (oneof)

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False,True]

genOrdering :: Gen Ordering
genOrdering = elements [LT,EQ,GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a,b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a,b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a,b,c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a,b,c)
  
genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]
  
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]
  
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [(1, return Nothing), (3, return (Just a))]
  
prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

--14.6

data Trivial = Trivial deriving (Eq, Show)
trivialGen :: Gen Trivial
trivialGen = return Trivial
 
instance Arbitrary Trivial where arbitrary = trivialGen
          
data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)
  
instance Arbitrary a => Arbitrary (Identity a) where arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

-- arbitrary products

data Pair a b = Pair a b deriving (Eq, Show)

pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)
  
instance (Arbitrary a, Arbitrary b) 
          => Arbitrary (Pair a b) 
          where
            arbitrary = PairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

-- arbitrary sum
data Sum a b = First a | Second b deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) 
                => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a, return $ Second b]
  
sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual
  
sumGenFirstPls :: (Arbitrary a, Arbitrary b)
                   => Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a), (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls  
