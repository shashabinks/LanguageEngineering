max2::Int->Int->Int
max2 x y 
    | x > y = x
    | y > x = y
    | otherwise = x

max3::Int->Int->Int->Int
max3 x y z
    | x > max y z = x
    | y > max x z = y
    | otherwise = z

isPositive::[Int]->Bool
isPositive [] = True
isPositive (x:xs)
        | x > 0 = isPositive xs
        | otherwise = False
    
isSorted::[Int]->Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x < y && isSorted(y:xs)


data IntTree = Br Int IntTree IntTree | Lf

count :: IntTree -> Int
count Lf = 0
count (Br n ls rs) = 1 + (count ls) + (count rs)

depth :: IntTree -> Int 
depth Lf = 0
depth (Br n ls rs) = 1 + max(depth ls) (depth rs)