{-# LANGUAGE FlexibleInstances #-}

module Week5.Week5 where

import Week5.ExprT
import Week5.Parser (parseExp)
import qualified Week5.StackVM as ST

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
    add = Add
    mul = Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

integify :: Integer -> Integer
integify = id

instance Expr Bool where
    lit = (0<)
    add = (||)
    mul = (&&)

boolify :: Bool -> Bool
boolify = id

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

minMaxify :: MinMax -> MinMax
minMaxify = id

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 (mod x 7)
  add (Mod7 x) (Mod7 y) = lit $ x + y
  mul (Mod7 x) (Mod7 y) = lit $ x * y

mod7ify :: Mod7 -> Mod7
mod7ify = id

instance Expr ST.Program where
  lit num = [ST.PushI num]
  add exp1 exp2  = exp1 ++ exp2 ++ [ST.Add]
  mul exp1 exp2  = exp1 ++ exp2 ++ [ST.Mul]

compile :: String -> Maybe ST.Program
compile = parseExp lit add mul
