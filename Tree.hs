module Tree where

data Tree a = Node a (Tree a) (Tree a) | Leaf a
    deriving (Show)

mount :: Tree [Char] -> [Char]
mount (Leaf x) = x
mount (Node x l r) = mount l ++" "++ mount r ++" "++ x

