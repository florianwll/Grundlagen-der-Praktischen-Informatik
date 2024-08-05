import Data.Maybe (isJust, fromJust)

-- Match a specific character and return the rest of the string if successful
match :: Char -> Maybe String -> Maybe String
match _ Nothing = Nothing
match c (Just (x:xs))
  | c == x    = Just xs
  | otherwise = Nothing
match _ (Just []) = Nothing

-- Function to parse prog -> expr '$'
prog :: String -> Maybe String
prog input = case expr (Just input) of
  Just ('$':rest) -> Just rest
  _               -> Nothing

-- Function to parse expr -> term ttail
expr :: Maybe String -> Maybe String
expr input = term input >>= ttail

-- Function to parse term -> factor ftail
term :: Maybe String -> Maybe String
term input = factor input >>= ftail

-- Function to parse ttail -> '+' term ttail | ε
ttail :: Maybe String -> Maybe String
ttail input = (match '+' input >>= term >>= ttail) <|> input

-- Function to parse factor -> 'c'
factor :: Maybe String -> Maybe String
factor input = match 'c' input

-- Function to parse ftail -> '*' factor ftail | ε
ftail :: Maybe String -> Maybe String
ftail input = (match '*' input >>= factor >>= ftail) <|> input

-- Test the parser
main :: IO ()
main = do
  print $ prog "c+c*c$" -- Should print Just ""
  print $ prog "c+c-c$" -- Should print Nothing

-- Helper function for alternative choices (<|>)
(<|>) :: Maybe a -> Maybe a -> Maybe a
Nothing <|> r = r
l       <|> _ = l
