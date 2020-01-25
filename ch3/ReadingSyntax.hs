--
-- Chapter Exercises: Reading syntax
--

a1 = concat [[1,2,3],[4,5,6]]
b1 = (++) [1,2,3] [4,5,6]
c1 = (++) "hello" " world"
d1 = ["hello" ++ " world"]
e1 = "hello" !! 4
f1 = (!!) "hello" 4
g1 = take 4 "lovely"
h1 = take 3 "awesome"

a2 = concat [[1*6],[2*6],[3*6]] -- = concat [[6],[12],[18]] = [6,12,18]
b2 = "rain" ++ drop 2 "elbow" -- = "rain" ++ "bow" = "rainbow"
c2 = 10 * head [1,2,3] -- = 10 * 1 = 10
d2 = (take 3 "Julie") ++ (tail "yes") -- = "Jul" ++ "es" = "Jules"
e2 = concat [tail [1,2,3], tail [4,5,6], tail [7,8,9]]
      -- = concat [[2,3], [5,6], [8,9]] = [2,3,5,6,8,9]