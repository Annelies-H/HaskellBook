-- we are deriving Ord and Show (print) here. 
-- Ord can be derived because Ord is a subset of Eq and Eq is defined below
-- The days will be ordered from left (lowest) to right (highest), so Sun > Mon 
data DayOfWeek = 
  Mon | Tue | Weds | Thu | Fri | Sat | Sun 
  deriving (Ord, Show)

-- instances are read top down, so only if the first does not apply it will test for the second
-- therefore the false pattern is last, if it were first everything would be false
instance Eq DayOfWeek where
  (==) Mon  Mon  = True
  (==) Tue  Tue  = True
  (==) Weds Weds = True
  (==) Thu  Thu  = True
  (==) Fri  Fri  = True
  (==) Sat  Sat  = True
  (==) Sun  Sun  = True
  (==) _    _    = False

-- day of week and numerical day of month
data Date = 
  Date DayOfWeek Int 
  deriving (Ord, Show)

--the dates are equal if all of their constituent values are equal
--the compiler already expects the arguments to be of type DayOfWeek and type Int
instance Eq Date where
  (==) (Date weekday dayOfMonth) 
       (Date weekday' dayOfMonth') = 
    weekday == weekday' && dayOfMonth == dayOfMonth'