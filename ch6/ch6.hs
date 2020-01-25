



chk f a b = (f a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i x = fromInteger i + f x
