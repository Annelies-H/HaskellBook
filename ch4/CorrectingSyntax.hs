--1
x = (+)
f xs = w `x` 1
  where w = length xs
  
--2
ex2 = (\x -> x)

--3
ex3 = (\(x:xs) -> x)
ex3' (x:xs) = x
ex3'' = (\x -> head x)
