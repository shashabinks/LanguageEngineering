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