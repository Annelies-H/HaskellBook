module MaybeAnotherMonoid where

import Control.Monad
import Data.Monoid
import Test.QuickCheck
import OptionalMonoid
import MonoidCheck

newtype First' a = 
  First' {getFirst' :: Optional a}
  deriving (Eq, Show)
  
instance Arbitrary a => Arbitrary (First' a) where
  --arbitrary :: Gen (First' a)
  arbitrary = do
      x <- arbitrary
      return $ First' x
  
instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Only x)) _ = First' {getFirst' = Only x}
  mappend _ (First' (Only x)) = First' {getFirst' = Only x}
  mappend _ _ = First' {getFirst' = Nada} 
  
firstMappend :: First' a
             -> First' a
             -> First' a
             
firstMappend = mappend

type FirstMappend = 
     First' String
  -> First' String
  -> First' String
  -> Bool
  
type FstId =
  First' String -> Bool
  
main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)