
data Tree a = Br a (Tree a) (Tree a) | Lf
            deriving Show

linear :: Int -> Tree Int
linear 0 = Lf
linear 1 = (Br 1 Lf Lf)
linear n = (Br n Lf (linear (n-1)))

inorder :: Tree a -> [a]
inorder Lf = []
inorder (Br n ls rs) = inorder ls ++ [n] ++ inorder rs 

preorder :: Tree a -> [a]
preorder Lf = []
preorder (Br n ls rs) = preorder ls ++ preorder rs ++ [n]