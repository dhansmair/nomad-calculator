{-
    Module to define all the builtin math stuff
-}

module Builtins ( stdEnv ) where

import Definitions
import Environment
import Debug.Trace
import Simplification (simplify)


nanMsg = Left $ RuntimeError "builtin function requires input of type Num"
numArgMsg = Left $ RuntimeError "wrong number of arguments to [builtin]"


-- create the environment,
-- including standard functions etc
stdEnv :: Env
stdEnv = 
    [ 
    -- # functions are internals that cannot be used directly from the command line.
    -- Those are needed for the typecheck of the binary operations.
      ("#add", addB,       Right (TComb [TNum, TNum, TNum]), [])
    , ("#sub", subB,       Right (TComb [TNum, TNum, TNum]), [])
    , ("#mul", mulB,       Right (TComb [TNum, TNum, TNum]), [])
    , ("#div", divB,       Right (TComb [TNum, TNum, TNum]), [])
    , ("#pow", powB,       Right (TComb [TNum, TNum, TNum]), [])

    -- default library functions
    , ("sin", sinB,        Right (TComb [TNum, TNum]), [])
    , ("cos", cosB,        Right (TComb [TNum, TNum]), [])
    , ("tan", tanB,        Right (TComb [TNum, TNum]), [])
    , ("exp", expB,        Right (TComb [TNum, TNum]), [])
    , ("ln" , lnB,         Right (TComb [TNum, TNum]), [])
    , ("log" , logB,       Right (TComb [TNum, TNum, TNum]), [])
    , ("root", rootB,      Right (TComb [TNum, TNum, TNum]), [])
    , ("sqrt", sqrtB,      Right (TComb [TNum, TNum]), [])

    -- other
    , ("abs", absB,        Right (TComb [TNum, TNum]), [])
    , ("max", maxB,        Right (TComb [TNum, TNum, TNum]), [])

    -- constants
    , ("e"  , Num (exp 1), Right TNum, [])
    , ("pi" , Num pi,      Right TNum, [])

    , ("derive", deriveB,  Right (TComb [TComb [TNum, TNum], TComb [TNum, TNum]]), [])
    , ("id"    , idB,      Right (TComb [TVar "a", TVar "a"]), [])
    ]


-- builtin functions
--
idB = Abs ["x"] (Var "x")
addB = makeExpr "add" $ makeBinary (+)
subB = makeExpr "sub" $ makeBinary (-)
mulB = makeExpr "mul" $ makeBinary (*)
divB = makeExpr "div" $ makeBinary (/)
powB = makeExpr "pow" $ makeBinary (**)

sinB = makeExpr "sin" $ makeUnary sin
cosB = makeExpr "cos" $ makeUnary cos 
tanB = makeExpr "tan" $ makeUnary tan
expB = makeExpr "exp" $ makeUnary exp
lnB  = makeExpr "ln"  $ makeUnary log
logB = makeExpr "log" $ makeBinary logBase
absB = makeExpr "abs" $ makeUnary abs
maxB = makeExpr "max" $ makeBinary max

rootB = Abs ["n", "x"] (BinOp Pow (Var "x") (BinOp Div (Num 1) (Var "n")))
sqrtB = Abs ["x"] (BinOp Pow (Var "x") (Num 0.5))

deriveB = Builtin $ B "derive" deriveB'
    where 
        deriveB' :: [Expr] -> Either MyError Expr
        deriveB' [Abs [x] ex] = Right $ Abs [x] $ simplify $ deriveBy ex x
        deriveB' [Builtin (B name _)] = Right $ simplify $ deriveBy (Var name) "x"
        deriveB' list = Left $ RuntimeError $ "cannot derive " ++ show list

(#+) = BinOp Add 
(#-) = BinOp Sub
(#*) = BinOp Mul
(#/) = BinOp Div
(#^) = BinOp Pow


deriveBy :: Expr -> String -> Expr
deriveBy (Num n) s = Num 0
deriveBy (BinOp Add a b) s = deriveBy a s #+ deriveBy b s 
deriveBy (BinOp Sub a b) s = deriveBy a s #- deriveBy b s
deriveBy (BinOp Mul u v) s = (deriveBy u s #* v) #+ (u #* deriveBy v s)
deriveBy (BinOp Div u v) s = ((deriveBy u s #* v) #- (u #* deriveBy v s)) #/ (v #^ Num 2)
deriveBy (BinOp Pow f g) s = (f #^ g) #* (l #+ r)
    where
        l = deriveBy g s #* App (Var "ln") [f]
        r = deriveBy f s #* (g #/ f)

deriveBy (Var "sin") _ = cosB
deriveBy (Var "cos") _ = Abs ["x"] $ Num (-1) #* App (Var "sin") [Var "x"]
deriveBy (Var "tan") _ = Abs ["x"] $ Num 1 #/ App (Var "cos") [Var "x"] #^ Num 2
deriveBy (Var "ln" ) _ = Abs ["x"] $ Num 1 #/ Var "x"
deriveBy (Var "exp") _ = expB
deriveBy (Var x)     s = if x == s then Num 1 else Num 0

-- chain rule, nachdifferenzieren
-- deriveBy (App (Var f) [x]) s = App (deriveBy (Var f) s) [x] #* deriveBy x s
-- lazy evaluation
deriveBy (App (Var f) [x]) s = App (App (Var "derive") [Var f]) [x] #* deriveBy x s

deriveBy ex s = error $ "cannot derive " ++ show ex 


-- some helper functions
--

makeExpr :: String -> ([Expr] -> Either MyError Expr) -> Expr
makeExpr s f = Builtin $ B s f

makeUnary :: (Double -> Double) -> [Expr] -> Either MyError Expr
makeUnary f [Num x] = Right $ Num (f x)
makeUnary f [_] = nanMsg
makeUnary f _ = numArgMsg

makeBinary :: (Double -> Double -> Double) -> [Expr] -> Either MyError Expr
makeBinary f [Num a, Num b] = Right $ Num (f a b) 
makeBinary f [_, _] = nanMsg
makeBinary f _ = numArgMsg

makeTernary :: (Double -> Double -> Double -> Double) -> [Expr] -> Either MyError Expr
makeTernary f [Num a, Num b, Num c] = Right $ Num (f a b c) 
makeTernary f [_, _, _] = nanMsg
makeTernary f _ = numArgMsg

