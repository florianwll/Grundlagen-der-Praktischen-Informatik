{-# LANGUAGE ViewPatterns #-}

import Data.List (sort)

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

-- Der Idle-Prozess
idle :: Prozess
idle = Prozess { pid = "IDLE", arrival = -1, computing = -1 }

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

-- Hilfsfunktion zum Partitionieren der Prozessliste
partitionByArrival :: Int -> [Prozess] -> ([Prozess], [Prozess])
partitionByArrival currentTime = foldr partition ([], [])
  where
    partition p (arrived, notArrived)
      | arrival p <= currentTime = (p : arrived, notArrived)
      | otherwise                = (arrived, p : notArrived)

-- Funktion zum Aktualisieren der wartenden Prozesse (FCFS)
update_ready :: State -> State
update_ready state = state { new = notArrived, ready = ready state ++ arrived }
  where
    currentTime = time state
    (arrived, notArrived) = partitionByArrival currentTime (new state)

-- Funktion zum Aktualisieren des rechnenden Prozesses
update_run :: State -> State
update_run state
    | run state == idle && not (null (ready state)) =
        state { run = head (ready state), ready = tail (ready state) }
    | otherwise = state

-- Funktion zum Aktualisieren des Status für den nächsten Zeitabschnitt
update_time :: State -> State
update_time state = state { 
    time = newTime, 
    run = newRun, 
    chart = newChart, 
    ready = newReady 
  }
  where
    currentRun = run state
    newTime = time state + 1
    newChart = chart state ++ pid currentRun ++ " "
    updatedRun = currentRun { computing = computing currentRun - 1 }
    (newRun, newReady)
      | computing updatedRun <= 0 = (idle, ready state)
      | otherwise                 = (updatedRun, ready state)

-- Funktion, um das Scheduling-Verfahren FCFS zu simulieren
simulateFCFS :: State -> State
simulateFCFS state
    | allProcessesDone state = state
    | otherwise = simulateFCFS (update_time . update_run . update_ready $ state)

-- Hilfsfunktion zum Überprüfen, ob alle Prozesse abgeschlossen sind
allProcessesDone :: State -> Bool
allProcessesDone state = null (new state) && null (ready state) && run state == idle

-- Funktion zum Aktualisieren der wartenden Prozesse (SJF)
update_ready_sjf :: State -> State
update_ready_sjf state = state { new = notArrived, ready = sort (ready state ++ arrived) }
  where
    currentTime = time state
    (arrived, notArrived) = partitionByArrival currentTime (new state)

-- Funktion zum Aktualisieren der wartenden Prozesse (SRTF)
update_ready_srtf :: State -> State
update_ready_srtf state = state { new = notArrived, ready = sort (ready state ++ arrived) }
  where
    currentTime = time state
    (arrived, notArrived) = partitionByArrival currentTime (new state)

-- Funktion zum Aktualisieren des rechnenden Prozesses (SRTF)
update_run_srtf :: State -> State
update_run_srtf state
    | null (ready state) = state
    | otherwise = state { run = newRun, ready = newReady }
  where
    sortedReady = sort (run state : ready state)
    newRun = head sortedReady
    newReady = tail sortedReady

-- Funktion, um das Scheduling-Verfahren SJF zu simulieren
simulateSJF :: State -> State
simulateSJF state
    | allProcessesDone state = state
    | otherwise = simulateSJF (update_time . update_run . update_ready_sjf $ state)

-- Funktion, um das Scheduling-Verfahren SRTF zu simulieren
simulateSRTF :: State -> State
simulateSRTF state
    | allProcessesDone state = state
    | otherwise = simulateSRTF (update_time . update_run_srtf . update_ready_srtf $ state)

main :: IO ()
main = do
    let ps = [ Prozess { pid = "P1", arrival = 0, computing = 6 }
             , Prozess { pid = "P2", arrival = 2, computing = 6 }
             , Prozess { pid = "P3", arrival = 4, computing = 5 }
             , Prozess { pid = "P4", arrival = 12, computing = 4 }
             , Prozess { pid = "P5", arrival = 16, computing = 3 }
             , Prozess { pid = "P6", arrival = 19, computing = 6 }
             ]
    let initialState = State { new = ps, run = idle, ready = [], time = 0, chart = "" }

    -- Simulation für FCFS
    let finalStateFCFS = simulateFCFS initialState
    putStrLn $ "Final Schedule (FCFS): " ++ chart finalStateFCFS

    -- Simulation für SJF
    let finalStateSJF = simulateSJF initialState
    putStrLn $ "Final Schedule (SJF): " ++ chart finalStateSJF

    -- Simulation für SRTF
    let finalStateSRTF = simulateSRTF initialState
    putStrLn $ "Final Schedule (SRTF): " ++ chart finalStateSRTF
