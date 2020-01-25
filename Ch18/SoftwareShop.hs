module SoftwareShop where

-- years ago
type Founded = Int
-- number of programmers
type Coders = Int

data SoftwareShop =
  Shop {
      founded :: Founded
    , programmers :: Coders
   } deriving (Eq, Show)
   
data FoundedError =
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n
  
-- Tho, many programmers *are* negative.
validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 5000 = Left $ TooManyCoders n
  | otherwise = Right n
  
mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers
    
{-
Note that Either always short-circuits on the first thing to have 
failed. It must because in the Monad, later values can depend on 
previous ones. So, the problem is you can’t make a Monad for 
Validation that accumulates the errors like the Applicative does.
Instead, any Monad instance for Validation would be identical to 
Either’s Monad instance.
-}
