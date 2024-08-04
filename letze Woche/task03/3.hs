{-# LANGUAGE ViewPatterns #-}

import Text.Printf

-- Definition des Prozess-Typs
data Prozess = Prozess
    { pid :: String
    , arrival :: Int
    , computing :: Int
    } deriving (Show)

instance Eq Prozess where
    Prozess { computing = a } == Prozess { computing = b } = a == b

instance Ord Prozess where
    compare x y
        | computing x < computing y = LT
        | computing x > computing y = GT
        | otherwise                 = EQ

-- Definition des State-Typs
data State = State
    { new :: [Prozess]
    , run :: Prozess
    , ready :: [Prozess]
    , time :: Int
    , chart :: String
    }

instance Show State where
    show (State new run ready time chart) = 
        "Time: " ++ show time ++ "\n" ++
        "Running: " ++ show run ++ "\n" ++
        "New: " ++ unwords (map show new) ++ "\n" ++
        "Ready: " ++ unwords (map show ready) ++ "\n" ++
        "Chart: " ++ chart ++ "\n"

-- Beispiel zur Verwendung des State-Typs
main :: IO ()
main = do
    let process1 = Prozess { pid = "P1", arrival = 0, computing = 5 }
    let process2 = Prozess { pid = "P2", arrival = 2, computing = 3 }
    let process3 = Prozess { pid = "P3", arrival = 4, computing = 7 }
    let state = State
                { new = [process2, process3]
                , run = process1
                , ready = []
                , time = 0
                , chart = ""
                }
    print state