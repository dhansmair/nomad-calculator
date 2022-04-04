{-
this Module exports the function 'getType', which determines the type for a 
given expression.

The typecheck for our language is simpler than for haskell:
- only one data type: num
- no cyclic function calls: Since we do not have if/else, we cannot stop recursions,
  meaning cyclic function calls must be prohibited alltogether. 
  (during variable/function definition, we check for cycles)
  Thus we also don't need to type recursive supercombinators.

General strategy:
    in getType, first the type and a list of equations are determined using getTypeM.
    Then the most general unifier is computed from the equations.
    Lastly, the type variables are renamed to more readable names (a, b, c, ..)

Error cases:
    The typecheck fails if the expression cannot be typed, or if a variable is 
    not defined.

Disclaimer:
    There are no useful type error messages. The program only states that there
    is a type error.
    This would be a useful feature to implement in the future.

-}

module TypeCheck ( getType
                 , getTypePure
                 , getTypeEither ) where


import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Maybe ( mapMaybe )
import Data.List ( intercalate )

import Definitions
import Unification (unify, calcUnifier)

type TypeAssumption = (String, Type)
type MyState = ([TypeAssumption], [String], [TypeEquation])



-- the main function of this module.
-- parameters:
-- ex is the expression we want to type
-- gamma0 is the initial set of type assumptions, obtained from Env.
--  it contains the types for all builtins and user-defined functions.
-- returns Left String in case of failure with an error message, 
-- or Right type in case of success.
--
-- getType internally calls the monadic function getTypeM
getType :: (Monad m) => Expr -> MyException (EnvT m) Type
getType ex = do
    gamma0 <- lift getGamma
    let names  = map (\s -> 'a' : show s) (iterate (+1) 1)
    let state0 = (gamma0, names, [])
    (t, (_, _, equations)) <- runStateT (getTypeM ex) state0

    case unify t equations of 
        (Just t2) -> return $ substituteSaneNames t2
        Nothing -> throwE $ TypeError "unification failed"


-- this is a non-Monadic version of getType. 
-- It is required in some functions that do not know the EnvT monad
getTypePure :: Expr -> [TypeAssumption] -> Either MyError Type
getTypePure ex gamma0 = do
    let names  = map (\s -> 'a' : show s) (iterate (+1) 1)
    let state0 = (gamma0, names, [])
    res <- runExceptT $ runStateT (getTypeM ex) state0

    case res of
        Left err -> Left err
        Right (t, (_, _, equations)) -> 
            case unify t equations of 
                (Just t2) -> Right $ substituteSaneNames t2
                Nothing -> Left $ TypeError "unification failed"


-- another version of getType that returns an Either instead of throwing 
-- an exception
getTypeEither :: (Monad m) => Expr -> (EnvT m) (Either MyError Type)
getTypeEither ex = runExceptT (getType ex)


-- helper function to extract the initial type assumptions from EnvT
getGamma :: (Monad m) => EnvT m [TypeAssumption]
getGamma = do mapMaybe helper <$> get
    where
        helper (s, _, Right t, _) = Just (s, t)
        helper (_, _, Left _, _) = Nothing

-- helper functions to easier access and modify the state monad
-- get new type variable with a fresh name
getName :: (Monad m) => StateT MyState m Type
getName = do
    (gamma, names, eqs) <- get
    put (gamma, tail names, eqs)
    return (TVar (head names))

-- get n fresh type variables
getNames :: (Monad m) => Int -> StateT MyState m [Type]
getNames n = replicateM n getName

-- add a new type assumption to the state
addGamma :: (Monad m) => TypeAssumption -> StateT MyState m ()
addGamma g = do
    (gamma, nl, eqs) <- get
    put (g:gamma, nl, eqs)

-- add a new type equation to the state
addEquation :: (Monad m) => TypeEquation -> StateT MyState m ()
addEquation eq = do
    (gamma, nl, oldEqs) <- get
    put (gamma, nl, eq:oldEqs)
    
-- AxV / AxSK:
-- type should be already determined -> Lookup in gamma
getTypeM :: (Monad m) => Expr -> StateT MyState (MyException m) Type
getTypeM (Var x) = do
    (gamma, _, _) <- get
    case lookup x gamma of
        Nothing -> throwError $ UndefinedVariableError $ "Variable " ++ x ++ " undefined or invalid, cannot check type."
        Just t -> do
            case t of
              -- AxSK: if a variable is bound to an abstraction, it is a supercombinator.
              -- It is implicitly all-quantified in gamma. 
              -- So use substituteTypeVars to give the variables new names.
              (TComb list) -> do
                (g, nl, eqs) <- get
                let (t', rest) = substituteTypeVars t nl
                put (g, rest, eqs)
                return t'
              -- AxV: otherwise we have AxV, just return the type
              _ -> return t

-- AxK: we only have one type of constructor, which has the type num
getTypeM (Num x) = return TNum

-- transform binary operators into applications of supercombinators.
-- the type (which is always num -> num -> num) is then looked up in gamma
getTypeM (BinOp op a b) = getTypeM (App (Var (op2name op)) [a, b])
    where
        op2name :: Op -> String
        op2name Add = "#add"
        op2name Sub = "#sub"
        op2name Mul = "#mul"
        op2name Div = "#div"
        op2name Pow = "#pow"

-- RAbs: rule for abstractions
getTypeM (Abs ids ex) = do
    newTypeVars <- getNames (length ids)
    mapM_ addGamma $ zip ids newTypeVars
    t <- getTypeM ex
    return $ TComb $ newTypeVars ++ [t]

-- RApp: rule for application (1/3)
-- For the typing, applications with multiple arguments are decomposed into 
-- multiple partial applications. 
getTypeM (App ex (a:b:args)) = getTypeM $ transformApp ex (a:b:args)
    where
        transformApp :: Expr -> [Expr] -> Expr
        transformApp s [] = s
        transformApp s (a:args) = transformApp (App s [a]) args

-- RApp: rule for application (2/3)
-- After decomposition, each App has only one argument, and the simple RApp rule 
-- from the lecture can be used.
getTypeM (App s [t]) = do
    tau1 <- getTypeM s 
    tau2 <- getTypeM t
    alpha <- getName
    addEquation (tau1, TComb [tau2, alpha]) 
    return alpha


-- RApp: rule for application (3/3)
-- this special case occurs when a function is invoked without arguments.
-- e.g., f() has the type of f
getTypeM (App s []) = getTypeM s

-- this case should not happen, because the types of all builtins are in gamma0
-- and thus simpliy looked up
getTypeM (Builtin _) = error "trying to check type of builtin, this should not happen"


-- helper function for the renaming of variables.
-- used to give new names to all-quantified type assumptions in gamma,
-- and to subsitute type variable names with readable ones in the end.
substituteTypeVars :: Type -> [String] -> (Type, [String])
substituteTypeVars t names = 
    let (t', (_, rest)) = runState (subst t) ([], names)
    in (t', rest)
    where
        subst :: Type -> State ([(String, String)], [String]) Type
        subst TNum = return TNum
        subst (TVar s) = do
            (replacings, list) <- get
            case lookup s replacings of
              (Just n) -> return $ TVar n
              Nothing -> do
                  let x = head list
                  put ((s,x):replacings, tail list)
                  return $ TVar x
        subst (TComb list) = do
            list' <- mapM subst list
            return $ TComb list'


-- helper function that replaces type variables with easier to read ones,
-- i.e. replaces a1, a2, ... an 
--          with a, b, c, ... a2, b2, ...
substituteSaneNames :: Type -> Type
substituteSaneNames t = fst $ substituteTypeVars t saneNames
    where
        saneChars = map (:[]) "abcdefghijklmnopqrstuvwxyz"
        saneNames = saneChars ++ [c ++ show n | n <- [2..], c <- saneChars]
