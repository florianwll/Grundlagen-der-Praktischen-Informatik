import Data.List (intercalate)

data Literal = Atom {atomChar :: Char} | NegA {atomChar :: Char} deriving (Eq)

instance Show Literal where
    show (Atom a) = [a]
    show (NegA a) = '!' : [a]

data Clause = Clause [Literal]
data CNF = CNF [Clause]

instance Show Clause where
    show (Clause literals) = "(" ++ intercalate " v " (map show literals) ++ ")"

instance Show CNF where
    show (CNF clauses) = intercalate " ^ " (map show clauses)

fromClause :: Clause -> [Literal]
fromClause (Clause literals) = literals

fromCNF :: CNF -> [Clause]
fromCNF (CNF clauses) = clauses

alphaL :: [Literal] -> Literal -> Bool
alphaL literals lit = lit `elem` literals

alphaC :: (Literal -> Bool) -> Clause -> Bool
alphaC alpha (Clause literals) = any alpha literals

alphaCNF :: (Literal -> Bool) -> CNF -> Bool
alphaCNF alpha (CNF clauses) = all (alphaC alpha) clauses

main :: IO ()
main = do
    let a = Clause [NegA 'A', Atom 'B']
    let b = Clause [NegA 'B', Atom 'C']
    let c = CNF [a, b]
    
    let alpha = alphaL [NegA 'A', Atom 'B', NegA 'C']
    
    print a             -- Output: (!A v B)
    print b             -- Output: (!B v C)
    print c             -- Output: (!A v B) ^ (!B v C)
    
    print $ alpha (Atom 'B')   -- Output: True
    print $ alpha (NegA 'B')   -- Output: False
    
    print $ alphaC alpha a     -- Output: True
    print $ alphaC alpha b     -- Output: False
    print $ alphaCNF alpha c   -- Output: False
    
    let allLiterals = [[a, b, c] | a <- [Atom 'A', NegA 'A'], b <- [Atom 'B', NegA 'B'], c <- [Atom 'C', NegA 'C']]
    let allAlphas = map alphaL allLiterals
    
    mapM_ (\alpha -> print (alphaCNF alpha c)) allAlphas
