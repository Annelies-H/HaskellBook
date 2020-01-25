--
-- Chapter 3 Chapter Exercises: Building Functions
--

-- 1

a1 = "Curry is awesome" ++ "!"
b1 = (!!) "Curry is awesome" 4  -- (!!) starts counting at 0
c1 = drop 9 "Curry is awesome"

-- 2

a2 string1 string2 = string1 ++ string2
b2 string1 x = (!!) string1 x
c2 x string1 = drop x string1

-- 3

thirdLetter :: String -> Char
thirdLetter x = (!!) x 2

-- 4

letterIndex :: Int -> Char
letterIndex x = (!!) string (x-1)     -- (x-1) so that 1 will return the first value, 2 the second, etc.
  where string = "Curry is awesome!"
  
-- 5
  
rvrs :: String
rvrs = (drop 9 string) ++ (take 4 (drop 5 string)) ++ (take 5 string)
  where string = "Curry is awesome"
  
-- 6: see separate file