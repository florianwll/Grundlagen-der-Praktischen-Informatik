import Data.Char (toLower)

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
foldList f[x] = x
foldList f(x:xs) = f x (foldList f xs)

mapList :: ( Int -> Int ) -> [ Int ] -> [ Int ]
mapList f [] = []
mapList f (x:xs) = f x : mapList f xs

tableInt :: (Int -> Int) -> [Int] -> String
tableInt f [] = ""
tableInt f [x] = show x ++ ":" ++ show (f x)
tableInt f (x:xs) = show x ++":"++ show (f x) ++ "\n" ++ tableInt f xs


containsList :: [ Int ] -> Int -> Bool
containsList [] _ = False
containsList (x:xs) n | x == n = True
                      | otherwise = containsList xs n

countList :: [Char] -> Char -> Int
countList [] _ = 0
countList (x:xs) n | toLower x == n = 1 + countList xs n
                   | otherwise = countList xs n


