data BinaryTree a = 
   Leaf
   |Node (BinaryTree a) a (BinaryTree a)
   deriving (Eq, Ord, Show)
   


--Write map for BinaryTree   
   
mapTree :: (a->b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
   Node (mapTree f left) (f a) (mapTree f right)
   
testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = if mapTree (+1) testTree' == mapExpected
          then print "yup okay!"
          else error "test failed!"
          
--Convert binary trees to lists
preorder :: BinaryTree a -> [a]
preorder (Node Leaf a Leaf) = [a]
preorder (Node Leaf a right) = [a] ++ preorder right
preorder (Node left a Leaf) = [a] ++ preorder left 
preorder (Node left a right) = [a] ++ preorder left ++ preorder right
  
inorder :: BinaryTree a -> [a]
inorder (Node Leaf a Leaf) = [a]
inorder (Node Leaf a right) = [a] ++ inorder right
inorder (Node left a Leaf) = inorder left ++ [a]
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder (Node Leaf a Leaf) = [a]
postorder (Node Leaf a right) = postorder right ++ [a]
postorder (Node left a Leaf) = postorder left ++ [a]
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2,1,3]
               then putStrLn "Preorder fine!"
               else putStrLn "Bad news bears."
               
testInorder :: IO ()
testInorder = if inorder testTree == [1,2,3]
               then putStrLn "Inorder fine!"
               else putStrLn "Bad news bears."
               
testPostorder :: IO ()
testPostorder = if postorder testTree == [1,3,2]
               then putStrLn "Postorder fine!"
               else putStrLn "Bad news bears."

  -- Extra test to test for nodes with one Leaf and one new Node.
testTree2 :: BinaryTree Integer
testTree2 = Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 (Node Leaf 4 (Node Leaf 5 Leaf))

testPreorder2 :: IO ()
testPreorder2 = if preorder testTree2 == [3,2,1,4,5]
               then putStrLn "Preorder2 fine!"
               else putStrLn "Bad news bears."
               
testInorder2 :: IO ()
testInorder2 = if inorder testTree2 == [1,2,3,4,5]
               then putStrLn "Inorder2 fine!"
               else putStrLn "Bad news bears."
               
testPostorder2 :: IO ()
testPostorder2 = if postorder testTree2 == [1,2,5,4,3]
               then putStrLn "Postorder2 fine!"
               else putStrLn "Bad news bears."               
main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder
  testPreorder2
  testInorder2
  testPostorder2
  
  
--Write foldr for BinaryTree
foldrTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTree _ z Leaf = z
foldrTree f z (Node left a right) = f a (foldrTree f (foldrTree f z right) left)  


  -- cheating by turning the tree into a list first
foldrTree' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTree' _ z Leaf = z
foldrTree' f z tree = go f z (inorder tree)
    where
      go _ z [] = z
      go f z (x:xs) = go f (f x z) xs


--Making a binary search tree from a random list

  -- The insert' function from the book (does not allow duplicates)
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)
  
  -- Use foldr and start with an empty tree. 
  -- Depending on how close the first value is to the median of the list you get a more or a less balanced tree
listToTree :: (Ord a) => [a] -> BinaryTree a
listToTree = foldr insert' Leaf

searchtree = listToTree [3,5,1,2,4]
unbalancedtree = listToTree [1..5]