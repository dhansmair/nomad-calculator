module Simplification where

import Definitions


simplify :: Expr -> Expr
simplify (Num n) = Num n
simplify (Var x) = Var x
simplify (BinOp Add a b) = 
    let a' = simplify a
        b' = simplify b
    in if a' == Num 0 then b'
       else if b' == Num 0 then a'
       else tryEval $ BinOp Add a' b'

simplify (BinOp Sub a b) = 
    let a' = simplify a
        b' = simplify b
    in if b' == Num 0 then a'
       else tryEval $ BinOp Sub a' b'

simplify (BinOp Mul a b) = 
    let a' = simplify a
        b' = simplify b
    in if a' == Num 1 then b'
       else if b' == Num 1 then a'
       else if (a' == Num 0) || (b' == Num 0) then Num 0 
       else tryEval $ BinOp Mul a' b'

simplify (BinOp Div a b) = 
    let a' = simplify a
        b' = simplify b
    in if b' == Num 1 then a'
       else tryEval $ BinOp Div a' b'


simplify (BinOp Pow a b) = 
    let a' = simplify a
        b' = simplify b
    in if a' == Num 1 then Num 1 
       else if b' == Num 1 then a'
       else if b' == Num 0 then Num 1 
       else tryEval $ BinOp Pow a' b'

simplify (Abs params ex) = Abs params $ simplify ex

simplify (App (Var "ln") [Var "e"]) = Num 1

simplify other = other


tryEval :: Expr -> Expr
tryEval (BinOp Add (Num a) (Num b)) = Num (a + b)
tryEval (BinOp Sub (Num a) (Num b)) = Num (a - b)
tryEval (BinOp Mul (Num a) (Num b)) = Num (a * b)
tryEval (BinOp Div (Num a) (Num b)) = Num (a / b)
tryEval (BinOp Pow (Num a) (Num b)) = Num (a ** b)
tryEval other = other
