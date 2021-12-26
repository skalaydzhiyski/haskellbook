import Data.List

data Tree a =
    Leaf
  | Node (Tree a) a (Tree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Leaf = Leaf
mapTree f (Node left value right) = 
  Node (mapTree f left) (f value) (mapTree f right)

testTree :: Tree Integer
testTree =
  Node (Node Leaf 1 Leaf)
  2
  (Node Leaf 3 Leaf)

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node left val right) = inorder left ++ (val : inorder right)

postorder :: Tree a -> [a]
postorder Leaf = []
postorder (Node left val right) = postorder left ++ postorder right ++ [val]

preorder :: Tree a -> [a]
preorder Leaf = []
preorder (Node left val right) = val : (preorder left ++ preorder right)

foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree f base t = foldr f base $ preorder t


main :: IO ()
main = undefined

