module MaybeLibrary where

-- 1.
-- Simple boolean checks for Maybe values
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

-- 2. 
-- The following is the Maybe catamorphism. You can turn a Maybe value into anything else with this.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _f Nothing  = x
mayybee _x f (Just y) = f y

-- 3.
-- In case you just want to provide a fallback value
fromMaybe :: a -> Maybe a -> a 
fromMaybe x y = mayybee x id y

fromMaybe' :: a -> Maybe a -> a 
fromMaybe' x Nothing  = x
fromMaybe' _x (Just y) = y

-- 4. 
-- Converting between List and Maybe
listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:_xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

-- 5.
-- For when we want to just drop the Nothing values from a listToMaybe
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs)  = catMaybes xs
catMaybes (Just x : xs) = x : catMaybes xs

-- 6.
-- You'll see this called "sequence" later
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe list = go list []
  where
    go [] result = Just (reverse result)
    go (Nothing:_xs) _ = Nothing
    go (Just x:xs) result = go xs (x:result)

flipMaybe' :: [Maybe a] -> Maybe [a]
flipMaybe' = go []
  where
    go result []  = Just (reverse result)
    go _ (Nothing:_xs) = Nothing
    go result (Just x:xs) = go(x:result) xs 


  -- this version only works if a has an instance of equal
flipMaybe'' :: Eq a => [Maybe a] -> Maybe [a]
flipMaybe'' list 
  | elem Nothing list = Nothing
  | otherwise = Just (catMaybes list) 
 