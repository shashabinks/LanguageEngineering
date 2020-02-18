
data Tree a = Br a (Tree a) (Tree a) | Lf
            deriving Show

linear :: Int -> Tree Int
linear 0 = Lf
linear 1 = (Br 1 Lf Lf)
linear n = (Br n Lf (linear (n-1)))

inorder :: Tree a -> [a]
inorder Lf = []
inorder (Br n ls rs) = inorder ls ++ [n] ++ inorder rs 

postorder :: Tree a -> [a]
postorder Lf = []
postorder (Br n ls rs) = postorder ls ++ postorder rs ++ [n]

ino :: Tree a -> [a] -> [a]
ino Lf acc = acc
ino (Br n t1 t2) acc = ino t1 (n:(ino t2 acc)) 

inorderAcc  t = ino t []

posto :: Tree a -> [a] -> [a]
posto Lf acc = acc
posto (Br n t1 t2) acc = (posto t1(posto t2 (v:acc))) 

postorderAcc  t = posto t []