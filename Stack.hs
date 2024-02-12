module Stack where

data Stack a = Stack | Item a (Stack a) 
    deriving (Show,Eq)

push :: a -> Stack a -> Stack a
push x stack = Item x stack

pop :: Stack a -> (Maybe a, Stack a)
pop Stack          = (,) Nothing  Stack
pop (Item a stack) = (,) (Just a) stack

peek :: Stack a -> Maybe a
peek Stack      = Nothing
peek (Item x _) = Just x

size :: Stack a -> Int
size Stack          = 0
size (Item _ stack) = 1 + size stack

download :: Stack a -> [a]
download Stack      = []
download (Item x s) = x : download s

