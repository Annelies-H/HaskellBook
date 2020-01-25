module GreetIfCool1 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
--  if cool
--  if cool coolness
    if coolness == "downright frosty yo"
     then putStrLn "eyyyyyy, what's shakin'?"
  else putStrLn "pshhhh"
--  where cool = coolness == "downright frosty yo"  
--  where cool v = v == "downright frosty yo"  