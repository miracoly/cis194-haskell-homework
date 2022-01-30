module Week5.Week5 where

import Week5.ExprT
import Week5.Parser (parseExp)
import GHC.IO.Handle.Types (Handle__)

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr = evalExpr . parseExp Lit Add Mul
    where
        evalExpr :: Maybe ExprT -> Maybe Integer
        evalExpr (Just expr) = Just (eval expr)
        evalExpr Nothing = Nothing

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit 
    mul = Mul
    add = Add 

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
    lit = id
    mul = (*)
    add = (+)

integify :: Integer -> Integer
integify = id

instance Expr Bool where
    lit = (0<) 
    mul = (&&)
    add = (||)

boolify :: Bool -> Bool
boolify = id