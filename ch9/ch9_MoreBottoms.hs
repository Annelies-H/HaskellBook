import Data.Bool

--exercise 6

--using the if then else statement
ex6If = map (\x -> if x == 3 then (-x) else (x)) 

--using Bool (see chapter exercises chapter 7)
ex6Bool = map (\x -> bool x (-x) (x==3))

