a5 = foldr (++) "" ["woot", "WOOT", "woot"]
b5 = foldr max 'a' "fear is the little death"
c5 = foldr (&&) True [False, True]
c5' = and [False, True]
c5'' = and (foldr (:) [] [False, True])
d5 = foldr (||) True [False, True] -- returns True if any one is True
d5' = foldr (||) True [False, False] -- so returns true because the base case is True
d5'' = foldr (||) False [False, False] -- none of them is True so it returns False
d5''' = foldr (||) True [] -- also works with an empty list becaue [] returns the base case, in this case True
e5 = foldl (++) "" (map show [1..5])
e5' = foldr ((++).show) "" [1..5]
f5 = foldl const 'a' [1..5]
g5 = foldl const 0 "tacos"
h5 = foldr (flip const) 0 "buritos"
i5 = foldl const 'z' [1..5]
i5' = foldr (flip const) 'z' [1..5]

{-
why const only works with foldl:
const :: b -> a -> b
foldr :: (a -> b -> b) -> b -> [a] -> b
foldl :: (b -> a -> b) -> b -> [a] -> b 
-}