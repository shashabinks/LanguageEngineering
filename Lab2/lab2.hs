
data Tree a = Br a (Tree a) (Tree a) | Lf
            deriving Show

linear :: Int -> Tree Int
linear 0 = Lf
linear 1 = (Br 1 Lf Lf)
linear n = (Br n  (linear(n-2)) (linear (n-1)))