import Data.Maybe (isJust, fromJust)

-- Match a specific character and return the rest of the string if successful
mattch :: Char -> Maybe String -> Maybe String
mattch _ Nothing = Nothing
mattch t (Just (x:xs))
    | t == x    = Just xs
    | otherwise = Nothing

-- Function to parse factor -> 'c'
factor :: Maybe String -> Maybe String
factor p = mattch 'c' p

-- Function to parse ftail -> '*' factor ftail | ε
ftail :: Maybe String -> Maybe String
ftail p
    | isJust (mattch '*' p) = ftail (factor (mattch '*' p))
    | otherwise = p

-- Function to parse term -> factor ftail
term :: Maybe String -> Maybe String
term p = ftail (factor p)

-- Function to parse ttail -> '+' term ttail | ε
ttail :: Maybe String -> Maybe String
ttail p
    | isJust (mattch '+' p) = ttail (term (mattch '+' p))
    | otherwise = p

-- Function to parse expr -> term ttail
expr :: Maybe String -> Maybe String
expr p = ttail (term p)

-- Function to parse prog -> expr '$'
prog :: String -> Maybe String
prog p = mattch '$' (expr (Just p))

-- Main function with test cases
main :: IO ()
main = do
    -- Test cases
    print $ prog "c+c*c$" -- Should print Just ""
    print $ prog "c+c-c$" -- Should print Nothing
    print $ prog "c*c+c$" -- Should print Just ""
    print $ prog "c$"     -- Should print Just ""
    print $ prog "cc*$"   -- Should print Nothing
    print $ prog "c*c*$"  -- Should print Just ""

