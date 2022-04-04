{-
this module exports only the evaluate function, and also the alphaRename
helper function, which needs to be done once in cli.hs, before the typeCheck 
can be performed.

alphaRename uses the strategy from the lecture script, but is implemented with
a state monad.

evaluate uses our universal Monad EnvT, where all currently available 
definitions (functions and variables) are stored.
evaluate always evaluates arguments before they are passed to abstractions.
It also supports partial evaluation, except for builtins.
evaluate of a Var fetches the value from EnvT, and then evaluates the value.
-}

module Interpreter( evaluate, alphaRename ) where

import Debug.Trace
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity 
import Control.Monad.Trans.State
import Definitions 
import Environment

evaluate :: Monad m => Expr -> MyException (EnvT m) Expr
evaluate (Num n) = return $ Num n
evaluate (Var s) = do
    valOrErr <- lift $ lookupValueT s
    val <- exceptify valOrErr
    evaluate val

evaluate (BinOp op e1 e2) = do
    n1 <- evaluate e1
    n2 <- evaluate e2
    exceptify $ applyBinOp op n1 n2

evaluate (Abs [] ex) = evaluate ex
evaluate (Abs ps ex) = return $ Abs ps ex

evaluate (Builtin b) = return $ Builtin b
evaluate (App ex []) = evaluate ex

evaluate (App ex (a:args)) = do
    f <- evaluate ex
    case f of 
        Builtin (B _ f) -> do
            -- evaluate builtin function
            -- no partial application possible
            results <- mapM evaluate (a:args)
            exceptify $ f results
        _ -> do
            a' <- evaluate a
            f' <- exceptify $ betaReduce (alphaRename f) a'
            evaluate (App f' args)

-- helper function which unwraps an either value or throws an exception
exceptify :: Monad m => (Either MyError Expr) -> MyException m Expr
exceptify eith = case eith of
    Right ex -> return ex
    Left err -> throwE err

-- betaReduce: small helper function for evaluate
-- intended to be only used on abstractions that still have remaining parameters
-- the RuntimeError would normally not occur here, this should already be cought 
-- by the type system
betaReduce :: Expr -> Expr -> Either MyError Expr
betaReduce (Abs (p:ps) ex) arg = return $ Abs ps (replaceWith arg p ex)
betaReduce other arg = Left $ RuntimeError "Left side of application is not an abstraction"

-- replaceWith obj -> matcher -> body
replaceWith :: Expr -> String -> Expr -> Expr
replaceWith arg s (Num n) = Num n
replaceWith arg s (Var x) = if x == s then arg else Var x
replaceWith arg s (BinOp op e1 e2) = BinOp op (replaceWith arg s e1) (replaceWith arg s e2)
replaceWith arg s (Builtin b) = Builtin b

replaceWith arg s (App ex args) = do 
    let ex' = replaceWith arg s ex
        args' = map (replaceWith arg s) args
     in App ex' args'

-- important: need to make sure s is not in params, should be the case
-- because of previous alpha-renaming
replaceWith arg s (Abs params ex) = Abs params (replaceWith arg s ex) 


alphaRename ex = evalState (alphaRenameM ex) ([], names)
    where
        names = map (\s -> 'x' : show s) (iterate (+1) 1)

        -- alpha renaming using a state Monad which stores a list of renamings
        -- (a,b) and a list of fresh variables
        alphaRenameM :: Expr -> State ([(String, String)], [String]) Expr
        alphaRenameM (Var v) = do
            (renamings, freshvars) <- get
            case lookup v renamings of
                Nothing -> return $ Var v
                Just v' -> return $ Var v'
        alphaRenameM (Num n) = return $ Num n
        alphaRenameM (BinOp op a b) = do
            a' <- alphaRenameM a
            b' <- alphaRenameM b
            return $ BinOp op a' b'
        alphaRenameM (App ex args) = do
            ex' <- alphaRenameM ex
            args' <- mapM alphaRenameM args
            return $ App ex' args'
        alphaRenameM (Abs params ex) = do
            (renamings, freshvars) <- get
            let n = length params
                newNames = take n freshvars
                restvars = drop n freshvars
                rens = zip params newNames
            put (reverse rens ++ renamings, restvars)
            ex' <- alphaRenameM ex

            -- important!!! remove rens again from the state after finishing
            -- this alphaRenameM call
            put (renamings, restvars)
            return $ Abs newNames ex' 
        alphaRenameM (Builtin b) = return $ Builtin b


-- helper function to evaluate binary operations on numbers
applyBinOp :: Op -> Expr -> Expr -> Either MyError Expr
applyBinOp op (Num a) (Num b) = Right $ Num $ applyBinOp' op a b
            where
                applyBinOp' Add a b = a + b
                applyBinOp' Sub a b = a - b
                applyBinOp' Mul a b = a * b
                applyBinOp' Div a b = a / b
                applyBinOp' Pow a b = a ** b

-- should be handled by the type system
applyBinOp op _ _ = Left $ RuntimeError $ "Tried to apply " ++ showEx op 
                            ++ " to wrong data types." 


