fibs :: [Int]
fibs = 1 : scanl (+) 1 fibs  -- gives an infinite list with Fibonacci numbers
fibsN :: Int -> Int
fibsN x = fibs !! x          -- gives the n'th Fibonacci number (where n = x + 1)

-- 1
fibs20 :: [Int]
fibs20 = take 20 fibs

-- 2
fibsLess100 :: [Int]
fibsLess100 = takeWhile (<100) fibs

-- 3

-- recursive function that gives the nth factorial (only positive numbers!)
recursedFactN :: Integer -> Integer
recursedFactN 0 = 1
recursedFactN n = go n (n-1)
    where 
      go acc 1 = acc
      go acc next = go (acc*next) (next - 1)

-- recursive function that gives a list of all the factorials, starting at 0!      
recursedFact :: [Integer]
recursedFact = 1 : go 1 1
   where
     go acc n = (acc * n) : go (acc*n) (n+1)

recursedFact5 :: [Integer]
recursedFact5 = take 6 recursedFact

-- recursive factorial function from the book
recursedFactBook :: Integer -> Integer
recursedFactBook 0 = 1
recursedFactBook n = n * recursedFactBook (n - 1)

-- factorial using scan
factorial = scanl (*) 1 [1..] 
factorial5 = take 6 factorial
factorialN n = scanl (*) 1 [1..n]

