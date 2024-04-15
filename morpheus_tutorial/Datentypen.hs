-- type Person = (String, String, Int)

-- Du :: Person
-- Du = ("Vorname", "Nachname", 42)


-- data Person = Mensch String String Int
-- -- Mensch :: String -> String -> Int -> Person

-- volljaehrig :: Person -> Bool
-- volljaehrig (Mensch vorname name alter) = alter >= 18

-- data schwierigkeit = Schwer String | Leicht String
-- data vorlesungsname = Mathe String | Programmieren String

-- vorlesung :: vorlesungsname -> schwierigkeit
-- vorlesung Mathe = Schwer
-- vorlesung Programmieren = Leicht

data Geometrie = Kreis Double | Rechteck Double Double | Quadrat Double
-- einheitskreis :: Geometrie
-- einheitskreis = Kreis 1.0

area :: Geometrie -> Double
area (Kreis r) = pi * r * r
area (Rechteck a b) = a*b
area (Quadrat a) = a*a