import Data.Time

data DatabaseItem = 
  DbString String | DbNumber Integer | DbDate UTCTime 
  deriving (Eq, Ord, Show)
  
theDatabase :: [DatabaseItem]
theDatabase = 
  [
  DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)), 
  DbNumber 9001, 
  DbString "Hello World",
  DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]
  
-- 1

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate database = foldr filterUTCTime [] database

filterUTCTime :: DatabaseItem -> [UTCTime] -> [UTCTime]
filterUTCTime a b = case a of
  DbDate x -> [x] ++ b
  _        -> b
  
-- 2

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber database = foldr filterInteger [] database

filterInteger :: DatabaseItem -> [Integer] -> [Integer]
filterInteger a b = case a of
  DbNumber x -> [x] ++ b
  _        -> b
  
-- 3
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent database = foldr max minDate (filterDbDate database)
  where 
    minDate = UTCTime (fromGregorian 1 1 1) (secondsToDiffTime 0)

mostRecent' :: [DatabaseItem] -> UTCTime
mostRecent' database = maximum (filterDbDate database) 
    
-- 4

sumDb :: [DatabaseItem] -> Integer
sumDb database = foldr (+) 0 (filterDbNumber database)

sumDb' :: [DatabaseItem] -> Integer
sumDb' database = sum (filterDbNumber database)

-- 5

avgDb :: [DatabaseItem] -> Double
avgDb database = total / entries
  where 
    total = fromInteger (sumDb database)
    entries = (fromIntegral . length . filterDbNumber) database
