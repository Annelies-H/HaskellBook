data TisAnInteger = 
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn x') =
    x == x'
    
data TwoIntegers =
  Two Integer Integer
  
instance Eq TwoIntegers where
  (==) (Two x y) (Two x' y') =
    x == x' && y == y'
    
data StringOrInt =
  TisAnInt Int | TisAString String
  
instance Eq StringOrInt where
  TisAnInt x == TisAnInt x' = 
    x == x'
  TisAString blah == TisAString blah' = 
    blah == blah'
    
data Pair a = 
  Pair a a
  
instance Eq a => Eq (Pair a) where
 Pair v w == Pair v' w' =
    v == v' && w == w'

data Tuple a b =
  Tuple a b
  
instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple v w == Tuple v' w' =
    v == v' && w == w'
    
data EitherOr a b =
  Hello a | Goodbye b
  
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello v == Hello v' =
    v == v'
  Goodbye w == Goodbye w' =
    w == w'
  

