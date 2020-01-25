module EitherLibrary where 

-- 1. 
-- Try to eventually arrive at a solution that uses foldr, even if the earlier functions don't
lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' (Left x : xs) = x : lefts' xs
lefts' (_:xs) = lefts' xs

leftsFoldr :: [Either a b] -> [a]
leftsFoldr list = foldr (\x -> (f x ++)) [] list
  where 
     f (Left x) = [x]
     f _        = []

leftsFoldr' :: [Either a b] -> [a]
leftsFoldr' list = foldr ((++) . f) [] list
  where 
     f (Left x) = [x]
     f _        = []

--     
-- 2.
--

rights' :: [Either a b] -> [b]
rights' = foldr (\x -> (f x ++)) []
  where
    f (Right x) = [x]
    f  _        = []

--
-- 3.
--

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' list = (lefts' list, rights' list)

--
-- 4.
--

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just (f x)
eitherMaybe' _  _        = Nothing

-- 5.
-- This is a general catamorphism for Either values

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fl _  (Left x)  = fl x
either' _  fr (Right x) = fr x

-- 6.
-- Same as before but use the either' function you just wrote

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f y = either' (const Nothing) (Just . f) y
  -- const :: a -> b -> a  <- returns only the first value
