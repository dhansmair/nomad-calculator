{-
This module exports the environment, a state transformer storing the environment and a number of monadic actions.

The environment Env contains a list of environment definitions EnvDef, tuples containing the name, expression, type and dependencies of definitions.

-}

module Environment( Env, EnvT, runEnvT, getEnvT, addDefnT, getTypeDefsT, getDepGraphT, reevalDepsT, lookupValueT) where

import Debug.Trace

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.State
import Data.List (intercalate, sort, union, (\\))
import Data.Maybe (catMaybes, mapMaybe)
import Definitions
import TypeCheck ( getTypePure )


-- Reach is the maximum distance from one node (eg. a) to another (eg. b) on a dependency graph
data Reach
    = ReachAt Int -- a is dependant on b, but n steps removed
    | Unrelated -- a is not dependant on b
    | CyclicDependencies -- a and b are part of a dependency cycle
    deriving (Show, Eq)

-- Unrelated < ReachAt 0 < ReachAt 100 < CyclicDependencies
instance Ord Reach where
    (<=) Unrelated _ = True
    (<=) _ Unrelated = False
    (<=) (ReachAt a) (ReachAt b) = a <= b
    (<=) _ CyclicDependencies = True
    (<=) CyclicDependencies _ = False

-- A node on the dependency graph
data DepGraphNode = DependencyGraphNode
    { name :: String
    , expr :: Expr
    , deps :: [String]
    , indDeps :: [String]
    , reach :: Reach
    , typ :: Either MyError Type
    }
    deriving (Show)

------------------------------ Monadic Actions ------------------------------

-- synonym to run the monad
runEnvT = runStateT 

-- returns entire env
getEnvT :: Monad m => EnvT m Env
getEnvT = get

-- adds a definition into the env
addDefnT :: Monad m => (String, Expr, Either MyError Type) -> EnvT m ()
addDefnT d = modify (addDefn d)
  where
    addDefn (s, ex, t) env = (s, ex, t, getDeps ex) : filter (\(sEnv, exEnv, tEnv, depEnv) -> sEnv /= s) env

-- reevaluates dependants of a definition
reevalDepsT :: Monad m => String -> EnvT m ()
reevalDepsT s = do
    dg <- getDepGraphT s
    let maxReach = getMaxReach (map reach dg) 0
    let dg2 = map updateCyclicals dg
    let dg3 = if maxReach > 0 then reevalGraph dg2 [1 .. maxReach] else dg2
    put $ depGraphToEnv dg3
  where
    updateCyclicals dgn
        | reach dgn == CyclicDependencies = dgn{typ = Left $ CyclicDependencyError ("Dependant on " ++ myDeps dgn)}
        | otherwise = dgn
    myDeps dgn = intercalate "," (indDeps dgn)

-- initializes dependency graph and calculates reach to a definition
getDepGraphT :: Monad m => String -> EnvT m [DepGraphNode] 
getDepGraphT s = do
    env <- get
    let initDG = envToDepGraph env s
    return $ calcReachGraph initDG [s]

-- returns names and types of valid definitions within env
getTypeDefsT :: Monad m => EnvT m [(String, Type)]
getTypeDefsT = getTypeDefs <$> get

-- returns expression if a specific definition is part of the environment or an error if not
lookupValueT :: Monad m => String -> EnvT m (Either MyError Expr)
lookupValueT s = do
    env <- get
    let res = lookup s $ map (\(s, e, t, d) -> (s, e)) env
    case res of
        Nothing -> return $ Left $ UndefinedVariableError $ "Variable " ++ s ++ " undefined."
        Just ex -> return $ Right ex

------------------------------ Utility ------------------------------

-- returns names of direct dependencies of an expression
getDeps :: Expr -> [String]
getDeps (Builtin _) = []
getDeps (Num _) = []
getDeps (Var s) = [s]
getDeps (BinOp _ a b) = getDeps a `union` getDeps b
getDeps (Abs ns ex) = (\\) (getDeps ex) ns
getDeps (App a bs) = foldl union (getDeps a) (map getDeps bs) --not sure about this one (seems to work though)

--
getTypeDefs :: Env -> [(String, Type)]
getTypeDefs = mapMaybe helper
  where
    helper (s, e, Right t, d) = Just (s, t)
    helper (s, e, Left err, d) = Nothing

-- returns largest reach, disregarding cycles
getMaxReach :: [Reach] -> Int -> Int
getMaxReach (r : rs) m = getMaxReach rs (biggerReach r m)
  where
    biggerReach (ReachAt n) m = if n > m then n else m
    biggerReach _ m = m
getMaxReach [] m = m

-- calculates reach from one node to all nodes in dependencyGraph
calcReachGraph :: [DepGraphNode] -> [String] -> [DepGraphNode]
calcReachGraph dg (x : xs) = do
    let xNode = head (filter (\dgn -> name dgn == x) dg) -- TODO (Georg): this could be more elegant
    let (dg', addAccs) = unzip (map (`calcReachGraphNode` xNode) dg)
    calcReachGraph dg' (xs `union` catMaybes addAccs)
calcReachGraph dg [] = dg

-- progresses reach from one node to another
calcReachGraphNode :: DepGraphNode -> DepGraphNode -> (DepGraphNode, Maybe String)
calcReachGraphNode a x
    | name x `notElem` deps a = (a, Nothing)
    | reach a >= incReach (reach x) = (a, Nothing)
    | name a `elem` indDeps x = (a{reach = CyclicDependencies}, Just (name a))
    | otherwise = (a{reach = incReach (reach x), indDeps = indDeps a `union` indDeps x}, Just (name a))
  where
    incReach (ReachAt n) = ReachAt (n + 1)
    incReach r = r

-- reevaluates graph in order of reach
reevalGraph :: [DepGraphNode] -> [Int] -> [DepGraphNode]
reevalGraph dg (l : ls) = do
    let newEnv = depGraphToEnv dg
    let dg' = map (reevalNode newEnv l) dg
    reevalGraph dg' ls
reevalGraph dg [] = dg

-- reevaluates node if has the correct reach
reevalNode :: Env -> Int -> DepGraphNode -> DepGraphNode
reevalNode env l dgn
    | reach dgn == ReachAt l = dgn{typ = getTypePure (expr dgn) (getTypeDefs env)}
    | otherwise = dgn

-- converts the dependencyGraph to an environment
depGraphToEnv :: [DepGraphNode] -> Env
depGraphToEnv = map depGraphNodeToDef
  where
    depGraphNodeToDef dgn = (name dgn, expr dgn, typ dgn, deps dgn)

-- converts the environment to a dependency graph
envToDepGraph :: Env -> String -> [DepGraphNode]
envToDepGraph env s = map (initDGN s) env
  where
    initDGN nam (s, ex, t, dep) =
            DependencyGraphNode
                { name = s
                , expr = ex
                , deps = dep
                , indDeps = dep
                , reach = if s == nam then ReachAt 0 else Unrelated
                , typ = t
                }
