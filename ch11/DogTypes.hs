data PugType = PugData

data HuskyType a = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a = 
  Husky a | Mastiff a
  deriving (Eq, Show)
  
-- 1
-- Doggies is a type constructor