inp :: String
inp = "Curry is awesome"
outp = inp ++ "!"

exercise = do
   putStrLn input1
   putStrLn input2
   putStrLn input3
   where input1 = "Curry is awesome" ++ "!"
         input2 = take 1 (drop 4 "Curry is awesome!")
         input3 = drop 9 "Curry is awesome!"
         
         
exercise2 = do
   putStrLn (inp ++ "!")
   putStrLn (take 1 (drop 4 outp))
   putStrLn (drop 9 outp)
   
exerciseA x = x ++ "!"
exerciseB x = take 1 (drop 4 x)
exerciseC x = drop 9 x
   
thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = inp !! x

rvrs = do
   putStrLn ((drop 9 inp) ++ (take 4(drop 5 inp)) ++ (take 5 inp))
   



