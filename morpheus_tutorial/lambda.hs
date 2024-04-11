f :: (Fractional a, Floating a) =>  a -> a
f x = cos x / x

f2 = \x -> cos x / x

map2 :: (s->t) -> [s] -> [t]
map2 f [] = []
map2 f (x:xs) = f x : map2 f xs
-- map2 (\x -> x*2) [1,2,3,4,5] !!

filter2 :: (t-> Bool) -> [t] -> [t]
filter2 pred [] = []
filter2 pred (x:xs) = if pred x  then x : filter2 pred xs
                        else filter2 pred xs

hintereinander f g = (\x -> f (g x))

add5 = map (5+)
