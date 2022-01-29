module Week5.Week5 where

import Week5.ExprT
import Week5.Parser (parseExp)

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

evalStr :: String -> Maybe Integer
evalStr = evalExpr . parseExp Lit Add Mul 
    where
        evalExpr :: Maybe ExprT -> Maybe Integer
        evalExpr (Just expr) = Just (eval expr)
        evalExpr Nothing = Nothing 