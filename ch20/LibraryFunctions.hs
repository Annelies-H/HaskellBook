module LibraryFunctions where

import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0 

sum'' :: (Foldable t, Num a) => t a -> a
sum'' = getSum.foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

product'' :: (Foldable t, Num a) => t a -> a
product'' = getProduct.foldMap Product

elem' :: (Functor t, Foldable t, Eq a) => a -> t a -> Bool
elem' x xs = foldr (||) False ys
  where
    ys = fmap (x==) xs

myElem :: Eq a => a -> [a] -> Bool
myElem x xs = foldr (\a b -> if a == x then True else b) False xs    
    
    
elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' x xs = foldr f False xs
  where
    f a b = if a == x then True else b
    
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr f Nothing
  where
    f = (\a b -> if b == Nothing then Just a else g a b)
    g = (\a b -> if Just a < b then Just a else b)
    
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr f Nothing
  where
    f = (\a b -> if b == Nothing then Just a else g a b)
    g = (\a b -> if Just a > b then Just a else b)
    
null' :: (Foldable t) => t a -> Bool
null' xs = case foldr (\a b -> True || b) False xs of
  False -> True
  True -> False
  
length' :: Foldable t => t a -> Int
length' = foldr (\a b -> 1 + b) 0

toList :: Foldable t => t a -> [a]
toList = foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap (id) 

foldMap' :: (Functor t, Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f xs = foldr (<>) mempty ys
  where
    ys = fmap f xs

foldMap'' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap'' f xs = foldr (\a b -> f a <> b) mempty xs
