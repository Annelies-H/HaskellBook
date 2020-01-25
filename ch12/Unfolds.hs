module Unfolds where



-- 1.
-- Write the function myIterate using direct recursion. 
-- Compare the behavior with the built-in iterate to gauge correctness.

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

-- 2.
-- Write the function my unfoldr using direct recursion.
-- Compare the behavior with the built-in unfoldr to gauge correctness.

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = (maybe [] fleft (f x)) ++ myUnfoldr f (maybe x fright (f x))
   where
     fleft  (a,_) = [a] 
     fright (_,b) = b
     
-- 3.
-- Rewrite myIterate into betterIterate using myUnfoldr 

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr fmaybe x
  where
    fmaybe y = Just (y, f y)