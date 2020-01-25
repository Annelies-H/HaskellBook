--
--Exercises: Square Cube
--

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

--exercise 1
myTuple1 = [(x,y) | x <- mySqr, y <- myCube]

--exercise 2
myTuple2 = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]

--exercise 3
howMany1 = length myTuple1

howMany2 = length myTuple2