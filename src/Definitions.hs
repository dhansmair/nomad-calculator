{-
This module contains most of the type definitions used across our code, 
that are required in multiple places.
It has no dependencies to other modules, which is convenient because it can
be imported from everywhere without creating cyclic imports.
-}
module Definitions where

import Data.List (intercalate)
import Control.Monad.Except
import Control.Monad.Trans.State


type EnvDef = (String, Expr, Either MyError Type, [String])
type Env = [EnvDef]
type EnvT = StateT Env



type MyException = ExceptT MyError

-- TypeEquation is used in TypeCheck.hs and Unification.hs
type TypeEquation = (Type, Type)

-- Type: the type an expression can have. 
-- Features num, polymorphic type variables and abstractions.
data Type = TNum | TVar String | TComb [Type]
    deriving(Eq)

instance Show Type where
    show TNum = "num"
    show (TVar s) = s
    show (TComb [el]) = show el
    show (TComb list) = "(" ++ intercalate " -> " (map show list) ++ ")"

-- Stmt and Expr are the two fundamental data declarations to which the user
-- input is parsed.
data Stmt = Def String Expr
          | Expr Expr
          deriving(Show)

data Expr = Num Double
          | Var String
          | BinOp Op Expr Expr
          | App Expr [Expr] 
          | Abs [String] Expr
          | Builtin Builtin 
          deriving(Show)

data Op = Add | Sub | Mul | Div | Pow
    deriving(Show, Eq)

-- Buitin is a special type for Expr, which is used to provide a bridge between
-- our simple language and builtin functions
data Builtin = B String ([Expr] -> Either MyError Expr)

instance Show Builtin where
    show (B s _) = s


instance Eq Expr where
    (==) (Num a) (Num b) = a == b
    (==) (Var x) (Var y) = x == y 
    (==) (BinOp op1 a1 b1) (BinOp op2 a2 b2) = op1 == op2 && a1 == a2 && b1 == b2
    (==) (App ex1 l1) (App ex2 l2) = ex1 == ex2 && l1 == l2
    (==) (Abs l1 ex1) (Abs l2 ex2) = False
    (==) (Builtin (B name1 _)) (Builtin (B name2 _)) = name1 == name2
    (==) _ _ = False
    

-- showEx: special class for our Expressions. The showEx function is used to
-- 'prettyprint' Expressions for the user, with the same Syntax as if it was 
-- typed in (outputs legal strings that could be parsed again with our grammar).
-- Used 'historically', because show was used for debugging.
-- Probably we could replace ShowEx with Show now.
class ShowEx a where
    showEx :: a -> String

instance ShowEx Expr where
    showEx (Num d) = show d
    showEx (Var s) = s
    showEx (BinOp op a b) = "(" ++ showEx a ++ " " ++ showEx op ++ " " ++ showEx b ++ ")"
    showEx (App (Var s) exprs) = s ++ "(" ++ pPrintList exprs ++ ")"
    showEx (App ex exprs) = "(" ++ showEx ex ++ ")(" ++ pPrintList exprs ++ ")"
    showEx (Abs ids ex) = '\\' : intercalate ", " ids ++ " -> " ++ showEx ex
    showEx (Builtin (B name _)) = name

instance ShowEx Op where
    showEx Add = "+"
    showEx Sub = "-"
    showEx Mul = "*"
    showEx Div = "/" 
    showEx Pow = "^"

-- helper function for showEx
pPrintList :: [Expr] -> String
pPrintList list = intercalate ", " $ map showEx list

-- custom error type used throughout our program
data MyError = BlankError String
             | ParseError String
             | TypeError String
             | RuntimeError String
             | CyclicDependencyError String
             | UndefinedVariableError String
             | InvalidCommandError String

instance Show MyError where
    show (BlankError s)   = "Error: " ++ s
    show (ParseError s)   = "Parse error: " ++ s
    show (TypeError s)    = "Type error: " ++ s
    show (RuntimeError s) = "Runtime error: " ++ s
    show (CyclicDependencyError s) = "Cyclic dependency error: " ++ s
    show (UndefinedVariableError s) = "Undefined variable error: " ++ s
    show (InvalidCommandError s) = "Invalid command: " ++ s
