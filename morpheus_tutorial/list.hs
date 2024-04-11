length2 l = if (null l) then 0 
            else 1 + (length2 (tail l))

length3 [] = 0
length3 (x:xs) = 1 + (length3 xs)

minimum2 [] = error "empty list"
minimum2 (x:[]) = x
minimum2 (x:xs) = min x (minimum2 xs)

append [] y = [y]
append (x:xs) y = x:(append xs y) 

reverse2 [] = []
reverse2 (x:xs) = reverse2 xs ++ [x]

--take nimmt n ersten nur
--drop entfernt n ersten