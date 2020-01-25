--{-# LANGUAGE NoMonomorphismRestriction #-}

-- TYPE

--a = (*9) 6
--b = head [(0, "doge"),(1, "kitteh")]
--c = head [(0 :: Integer , "doge"),(1,"kitteh")]
--d = if False then True else False
--e = length [1,2,3,4,5]
--f = (length [1,2,3,4]) > (length "TACOCAT")

--x = 5
--y = x + 5
--w = y * 10

--z y = y * 10
--f = 4 / y

--x = "Julie"
--y = " <3 "
--z = "Haskell"
--f = a ++ b ++ z

--FIX IT

--SING

--fstString :: [Char] -> [Char]
--fstString x = x ++ " in the rain"

--sndString :: [Char] -> [Char]
--sndString x = x ++ " over the rainbow"

--sing = if (x < y) then fstString x else sndString y
--  where x = "Singin"
 --       y = "Somewhere"
 
--Arith3Broken

main :: IO ()
main = do
  print (1 + 2)
  putStrLn "10"
  print (negate (-1))
  print ((+) 0 blah)
  where blah = negate 1

 
 
 
 
 
 
 