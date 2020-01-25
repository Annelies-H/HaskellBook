module BinaryTree2 where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

-- 1. 
-- Write unfold for BinaryTree
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of
    Nothing -> Leaf
    Just (a,b,c) -> Node (unfold f a) b (unfold f c)
    
-- 2.
-- Make a tree builder using the unfold function you've just made for BinaryTree.
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where
    f x 
     | x >= n = Nothing
     | otherwise = Just (x+1, x , x+1) 
    