import Data.Maybe

-- match überprüft, ob das nächste Symbol dem erwarteten entspricht
match :: String -> [String] -> [String]
match _ [] = error "match: unexpected end of input"
match expected (x:xs)
    | expected == x = xs
    | otherwise = error $ "match: expected " ++ expected ++ ", but got " ++ x

-- id_list verarbeitet eine "id" gefolgt von einem id_list_tail
id_list :: [String] -> [String]
id_list tokens = id_list_tail (match "id" tokens)

-- id_list_tail verarbeitet "," "id" id_list_tail oder ";" "$$"
id_list_tail :: [String] -> [String]
id_list_tail [] = error "id_list_tail: unexpected end of input"
id_list_tail tokens@(x:xs)
    | x == ","  = id_list_tail (match "id" xs)
    | x == ";"  = match "$$" xs
    | x == "$$" = [] -- Akzeptiere das Ende der Eingabe
    | otherwise = error $ "id_list_tail: unexpected token " ++ x

-- Funktion zum Überprüfen, ob die Eingabe vollständig verarbeitet wurde
parse :: [String] -> Bool
parse tokens = null (id_list tokens)

-- Beispiel zur Verwendung des Parsers
main :: IO ()
main = do
    let input1 = ["id", ",", "id", ";", "$$"]
    let input2 = ["id", "$$"]
    let input3 = ["id", ",", "id", ",", "id", ";", "$$"]
    let input4 = ["id", ";", "$$"]
    let input5 = ["id", ",", "id", ",", "id", ",", "id", ";", "$$"]
    
    -- Testen der Beispiele
    print $ parse input1 -- Erwartet True
    print $ parse input2 -- Erwartet True
    print $ parse input3 -- Erwartet True
    print $ parse input4 -- Erwartet True
    print $ parse input5 -- Erwartet True
