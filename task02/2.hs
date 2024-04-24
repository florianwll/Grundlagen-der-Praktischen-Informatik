{-# LANGUAGE ViewPatterns #-}
import Text.Printf

type Nibble = (Bool,Bool,Bool,Bool)



showNibble :: Nibble -> String
showNibble x = printf "%d%d%d%d   %d    %d" a b c d (a * 8 + r) (a * (-8) + r)
    where (fromEnum -> a,fromEnum -> b,fromEnum -> c,fromEnum -> d) = x
          r = b * 4 + c * 2 + d * 1

bitAdder :: Bool -> Bool -> Bool -> ( Bool , Bool )
bitAdder x y z | x && y = (True, z)
               | x || y = (z, not z) 
               | otherwise = (False, z) 



nibbleAdder :: Nibble -> Nibble -> ( Bool , Nibble )
nibbleAdder (a,b,c,d) (w,x,y,z) = (u4,(e4,e3,e2,e1))
    where (u1,e1) = bitAdder d z False
          (u2,e2) = bitAdder c y u1
          (u3,e3) = bitAdder b x u2
          (u4,e4) = bitAdder a w u3



tableAdder :: (Nibble -> Nibble -> (Bool,Nibble)) -> [( Nibble , Nibble )] -> String
tableAdder f [] = ""
tableAdder f ((a,b):xs) = printf "%s + %s = %s %s \n%s" (showNibble a) (showNibble b) c d  $ tableAdder f xs 
    where (show -> c,showNibble -> d) = f a b