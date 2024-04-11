f x = cos x / x

g x = f (x*x)

h x =   if (x == 0)
            then 1
        else h (x-1)

h2 x | x == 0 = 1
     | otherwise = h (x-1)

square x = x*x
hoch3 x = square $ x * x