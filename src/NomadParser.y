{
-- MyParser.hs is automatically generated from MyParser.y using the happy parser generator.

-- Description for MyParser.y:
-- this file contains the definition of the grammar.
-- In the beginning, the parser created a one-to-one corresponding concrete syntax tree for the grammar.
-- Now, the functionalities of the happy parser are used to directly create an abstract syntax tree with 
-- the data types 'Stmt' and 'Expr', as defined in Definitions.hs
--
-- The parser uses the Either Monad, and returns Left err if a parser error occurs.
--
-- One problem in the grammar was that function applications like "f(x,y)" have different semantic meanings:
-- In definitions like "f(x,y) = expr", anything inside f(..) is a binding. Here only variables (ids) must occur.
-- If used in expressions, like calling the function f with arguments "f(5, 2x)",
-- anything inside f(..) is an argument and can be any expression.
-- Because of this, we needed to distinguish between List (containing expressions) and IdList (containing only ids).
-- We thus introduce two variations of each rule: rule and ruleN, where rule can be with IDs and ruleN is without IDs.
-- For example: 
--      AtomN ::= num | "(" <expr> ")" | <app>
--      Atom ::= id | <AtomN>






module NomadParser( parse ) where

import Control.Monad.Trans.Except( ExceptT, throwE )
import Lexer ( lexer, Token(..) )
import Definitions ( Stmt(..), Expr(..), Op(..), MyError(..), MyException )

}
%name calc
%monad { Either MyError } { (>>=) } { return }
%tokentype { Token }
%error { parseError }
%token 
      num             { TokenNum $$ }
      id              { TokenId $$ }
      '='             { TokenEq }
      ','             { TokenComma }
      '+'             { TokenAdd }
      '-'             { TokenSub }
      '*'             { TokenMul }
      '/'             { TokenDiv }
      '^'             { TokenPow }
      '('             { TokenLB }
      ')'             { TokenRB }
      lam             { TokenLam }
      '->'            { TokenArr }
%%

Stmt    : Expr2                 { Expr $1 }
        | IdFunc '=' Expr2      { makeDef $1 $3 } 
        | id '=' Expr2          { Def $1 $3 }

IdFunc  : id '(' ')'            { IdFunc $1 [] }
        | id '(' IdList ')'     { IdFunc $1 (reverse $3) }

App     : IdFunc                        { idFunc2App $1 } 
        | id '(' List ')'               { App (Var $1) (reverse $3) } 
        | App '(' List ')'              { App $1 (reverse $3) }
        | App '(' IdList ')'            { App $1 (idlist2list (reverse $3)) }
        | '(' Expr2 ')' '(' List ')'    { App $2 (reverse $5) }
        | '(' Expr2 ')' '(' IdList ')'  { App $2 (idlist2list (reverse $5)) }

Abs     : lam IdList '->' Expr2         { Abs (reverse $2) $4 }

List    : Expr2N                { [$1] }
        | List ',' Expr2        { $3 : $1 }
        | IdList ',' Expr2N     { $3 : (idlist2list $1) }

IdList  : id                    { [$1] }
        | IdList ',' id         { $3 : $1 }

Expr2   : id                    { Var $1 }
        | Expr2N                { $1 }

Expr    : id                    { Var $1 }
        | ExprN                 { $1 }

Term2   : id                    { Var $1 }
        | Term2N                { $1 }

Term    : id                    { Var $1 }
        | TermN                 { $1 } 

Factor  : id                    { Var $1 } 
        | FactorN               { $1 }

Atom    : id                    { Var $1 }
        | AtomN                 { $1 }

Expr2N  : Abs                   { $1 } 
        | ExprN                 { $1 }

ExprN   : Expr '+' Term2        { BinOp Add $1 $3 }
        | Expr '-' Term2        { BinOp Sub $1 $3 }
        | Term2N                { $1 }

Term2N  : '-' Term2             { BinOp Mul (Num (-1.0)) $2 }
        | '+' Term2             { $2 }
        | TermN                 { $1 }

TermN   : Term '/' Factor       { BinOp Div $1 $3}
        | Term '*' Factor       { BinOp Mul $1 $3}
        | num Factor            { BinOp Mul (Num $1) $2 }
        | FactorN               { $1}

FactorN : Factor '^' Atom       { BinOp Pow $1 $3 }
        | AtomN                 { $1 }

AtomN   : num                   { Num $1 }
        | '(' Expr2 ')'         { $2 }
        | App                   { $1 } 


{

parseError :: [Token] -> Either MyError a 
parseError tokenList = Left $ ParseError $ "Parse error at " ++ show tokenList

data IdFunc = IdFunc String [String] deriving(Show)

idlist2list :: [String] -> [Expr]
idlist2list [] = []
idlist2list (x:xs) = (Var x) : (idlist2list xs)

idFunc2App :: IdFunc -> Expr 
idFunc2App (IdFunc i idList) = App (Var i) (idlist2list idList)

makeDef :: IdFunc -> Expr -> Stmt 
makeDef (IdFunc s idList) ex = Def s (Abs idList ex)



parse :: (Monad m) => String -> MyException m Stmt
parse s = do
    tokens <- lexer s
    let res = calc tokens
    
    case res of
        Left err -> throwE err
        Right stmt -> return stmt

}
