-- import Data.Foldable
-- import Data.Foldable1
sum2 [] = 0
sum2 (x:xs) = x + sum2 xs

sum3 :: [Integer] -> Integer
sum3 = foldr (+) 0

length3 xs = foldr (+) 0 (map (\x -> 1) xs)
length4 xs = foldr (\x n -> n+1) 0 xs

zipWith2 f [] [] = []
zipWith2 f (x:xs) (y:ys) = f x y : zipWith2 f xs ys  

hammingDistanz xs ys = sum (zipWith unterschied xs ys)
                        where unterschied x y = if(x == y ) then 0
                                            else 1
