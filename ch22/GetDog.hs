module GetDog where

import ReadingComprehension

newtype HumanName =
  HumanName String
  deriving (Eq, Show)
  
newtype DogName =
  DogName String
  deriving (Eq, Show)
  
newtype Address =
  Address String
  deriving (Eq, Show)

data Person = 
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)
  
data Dog =
  Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)
  
person :: String -> String -> String -> Person
person n d a = Person (HumanName n) (DogName d) (Address a)

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address
-- dogname is een functie (<- reader) die van een person de dogname retouneert
-- je mapt Dog in de dogname-reader en applied die dan op de address-reader

-- with the readermonad en do-syntax
getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy
  
getDogRMR :: Reader Person Dog
getDogRMR = Reader getDogR
