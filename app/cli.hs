{-
The main command line interface for the application.

Defines the main and loop functions, which handle the main transformer stack. 
Helper functions facilitate the handling of user inputs, execution of actions and output of the results.

-}
import System.IO
import System.Console.Haskeline
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.List

import Interpreter ( evaluate, alphaRename )
import NomadParser (parse)
import Builtins (stdEnv)
import TypeCheck ( getType, getTypeEither )
import Definitions
import Environment

-- Evaluates statements by evaluating expressions and managing definitions
performAction :: Stmt -> MyException (EnvT IO) ()
performAction (Expr ex) = do
    t <- getType (alphaRename ex)
    d <- evaluate ex
    liftIO $ putStrLn $ showEx d ++ "\t:: " ++ show t
performAction (Def s ex) = lift $ do
    typ <- getTypeEither (alphaRename ex)
    addDefnT (s,ex,typ)
    reevalDepsT s
    lift $ putStrLn "variable or function defined"

-- :t command. Prints the type of an expression
showType :: String -> MyException (EnvT IO) ()
showType line = do
    stmt <- parse line
    case stmt of
        (Expr ex) -> do
            t <- getType (alphaRename ex)
            liftIO $ putStr "type: "
            liftIO $ print t
        _ -> throwE $ BlankError $ "\"" ++ line ++ "\" is not an expression"

-- :env command. Prints the Environment.
printEnv :: EnvT IO ()
printEnv = do
    env <- getEnvT
    let lines = env
    lift $ printLines lines

-- helper to print lists
printLines (x:xs) = do 
    print x
    printLines xs
printLines [] = return ()

-- Handles user input
handleInput :: String -> MyException (EnvT IO) () 
handleInput line = do
    if ":t " `isPrefixOf` line then do
        showType (drop 3 line)
    else if ":t" == line then do
        throwE $ InvalidCommandError ":t needs an expression as an argument"
    else if ":env" `isPrefixOf` line then do
        lift printEnv
    else if ":" `isPrefixOf` line then do
        throwE (InvalidCommandError $ "command \"" ++ line ++ "\" does not exist")
    else do
        stmt <- parse line
        performAction stmt

-- Main loop of the cli app. Receives and delegates user input and restarts itself.
loop :: InputT (EnvT IO) ()
loop = do
    minput <- getInputLine "> "
    case minput of
        Nothing -> return ()
        Just ":q" -> return ()
        Just input -> do
            r <- lift $ runExceptT $ handleInput input
            case r of 
                Left err -> do
                    liftIO $ print err
                    loop
                Right _ -> loop

-- Initializes main loop and prints a welcome message.
main :: IO ()
main = do
    putStrLn welcomeText
    runEnvT (runInputT defaultSettings loop) stdEnv
    return ()
  where 
    welcomeText = "Hello! This is the Nomad Calculator! (type :q to quit) \n\
    \-----------------------------------------------------------------"
