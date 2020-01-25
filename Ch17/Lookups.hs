module Lookups where

import Data.List (elemIndex)

-- 1.
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

-- 2.

y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z
{- 
<$> en <*> zijn left associative, ze doen dus eerst het linkerdeel
<$> :: Functor f => a -> b -> f a -> f b
dus (,) <$> y :: Maybe (b -> (Integer, b)
<*> :: Applicative f = f (a -> b) -> f a -> f b
dus ((,) <$> y) <*> z :: Maybe (Integer, Integer)
Hij stopt dus eerst de a in Maybe a als eerste in de tuple
de tuple is daarmee een half toegepaste functie in een maybe
vervolgens combineert hij de Maybe functie met de maybe waarde
tot maybe nieuwe waarde
-}

-- 3.

v :: Maybe Int
v = elemIndex 3 [1,2,3,4,5]

w :: Maybe Int
w = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> v <*> w

-- 4.

xs = [1,2,3]
ys = [4,5,6]

m :: Maybe Integer
m = lookup 3 $ zip xs ys

n :: Maybe Integer
n = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> m <*> n




