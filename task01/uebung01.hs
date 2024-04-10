quadratic :: (Int , Int , Int ) -> Int -> Int
quadratic (a, b, c) x = a * x * x + b * x + c


square :: Int -> Int
square n | n < 0 = square(-n)
         | n == 0 = 0
         | otherwise = sum [2*i - 1 | i <- [1..n]]  

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

foldList :: ( Double -> Double -> Double ) -> [ Double ] -> Double
