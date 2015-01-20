data SimpExpr = Lit Int
              | Add SimpExpr SimpExpr
              | Mul SimpExpr SimpExpr

instance Show SimpExpr where
  show (Lit n) = show n
  show (Add x y) = (show x) ++ " + " ++ (show y)
  show (Mul x y) = (show x) ++ " * " ++ (show y)

reducible :: SimpExpr -> Bool
reducible (Lit n) = False
reducible _       = True

reduce :: SimpExpr -> SimpExpr
reduce (Lit n) = Lit n
reduce (Add (Lit x) (Lit y)) = Lit (x + y)
reduce (Mul (Lit x) (Lit y)) = Lit (x * y)
reduce (Add x y) | reducible x = Add (reduce x) y
                 | reducible y = Add x (reduce y)
reduce (Mul x y) | reducible x = Mul (reduce x) y
                 | reducible y = Mul x (reduce y)

runMachine :: SimpExpr -> IO ()
runMachine (Lit n) = putStrLn (show n)
runMachine e       = putStrLn (show e) >> runMachine (reduce e)

