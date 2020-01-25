data DayOfWeek = 
  Mon | Tue | Weds | Thu | Fri | Sat | Sun 
  deriving (Eq, Show)

-- instances are read top down, so only if the first does not apply it will test for the second
-- Wednesday is always the best (greater) day of the week, all the other days are equal but less than wednesday
-- we derived Eq above, so compare Thu Sun = EQ but Thu == Sun = False (they are equal but not the same)
instance Ord DayOfWeek where
  compare Weds Weds = EQ
  compare Weds _    = GT
  compare _    Weds = LT
  compare _    _    = EQ

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