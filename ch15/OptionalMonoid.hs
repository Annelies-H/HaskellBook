module OptionalMonoid where

import Data.Monoid
import Test.QuickCheck

data Optional a = Nada | Only a
                    deriving (Eq, Show)
                   
instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) (Only y) = Only (mappend x y)
  mappend (Only x) Nada = Only (mappend x mempty)
  mappend Nada (Only y) = Only (mappend mempty y)
  
instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = do
      a <- arbitrary
      frequency [(1, return Nada), (3, return (Only a))]