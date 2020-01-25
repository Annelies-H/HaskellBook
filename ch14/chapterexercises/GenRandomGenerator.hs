module GenRandomGenerator where

import Test.QuickCheck 
import Test.QuickCheck.Gen (oneof)

data Fool = Fulse | Frue deriving (Eq, Show)

genFool1 :: Gen Fool
genFool1 = elements [Fulse, Frue]

sampleFool1 :: IO ()
sampleFool1 = sample genFool1

genFool2 :: Gen Fool
genFool2 = frequency [(2, return Fulse), (1, return Frue)]

sampleFool2 :: IO()
sampleFool2 = sample genFool2

