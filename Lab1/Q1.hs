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