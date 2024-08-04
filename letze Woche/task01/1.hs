import Data.Char (toLower )
quadratic :: (Int, Int, Int) -> Int -> Int
quadratic (a, b, c) x = a * x ^ 2 + b * x + c

sumTupel :: (Int, Int) -> Int
sumTupel (a, b) = a + b

square :: Int -> Int
square n
  | n < 0     = square (-n)
  | n == 0    = 0
  | otherwise = sum [2*i - 1 | i <- [1..n]]

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

foldList :: (Double -> Double -> Double) -> [Double] -> Double
foldList _ [] = 0
foldList a (x:xs) = a x (foldList a (xs))

mapList :: (Int -> Int ) -> [Int] -> [Int]
mapList _ [] = []
mapList f (x:xs) = f x : mapList f xs

tableInt :: (Int -> Int) -> [Int] -> String
tableInt f [] = ""
tableInt f [x] = show x ++ ":" ++ show (f x)
tableInt f (x:xs) = show x ++":"++ show (f x) ++ "\n" ++ tableInt f xs

containsList :: [Int] -> Int -> Bool
containsList [] _ = False
containsList (x:xs) a
    | x == a    = True
    | otherwise = containsList xs a


countList :: [Char] -> Char -> Int
countList [] _ = 0
countList (x:xs) a
    | toLower x == toLower a = 1 + countList xs a
    | otherwise = countList xs a