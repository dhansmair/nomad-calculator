{-
this module provides the functions unify and calcUnifier.
-}


module Unification ( unify 
                   , calcUnifier
                   ) where


import Control.Monad ( (>=>) )
import Data.List (intercalate, find)
import Data.Maybe (fromMaybe)

import Definitions ( Type(..)
                   , TypeEquation 
                   )


-- |takes a type t and a list of type equations eqs. 
-- The type equations are unified,
-- and the most general unifier of t (sigma(t)) is returned on success.
-- returns Nothing if the unification fails.
unify :: Type -> [TypeEquation] -> Maybe Type
unify t eqs = do
    unifier <- calcUnifier eqs
    return $ replaceVariables t unifier
      where
        replaceVariables :: Type -> [TypeEquation] -> Type
        replaceVariables TNum _        = TNum
        replaceVariables (TVar x)  eqs = fromMaybe (TVar x) (lookup (TVar x) eqs)
        replaceVariables (TComb t) eqs = TComb $ map (`replaceVariables` eqs) t



-- |expects a list of type equations as input and outputs a simplified list of
-- type equations, or nothing if one of the checks fails.
-- 
-- The function works by repeatedly doing 'elim', 'failCheck' and 'occursCheck'
-- and then trying to perform one rule of ['orient', 'solve', 'decompose'].
-- If no more rule can be applied, the list of remaining equations is returned.
calcUnifier :: [TypeEquation] -> Maybe [TypeEquation]
calcUnifier eqs = do

    -- simplify equations
    let eqs1 = map (\(x,y) -> (simplifyType x, simplifyType y)) eqs
    -- elim
        eqs2 = filter (\(x,y) -> x/=y) eqs1

    -- checks
    eqs3 <- mapM (failCheck >=> occursCheck) eqs2

    if any canOrient eqs3
        -- orient rule
        then calcUnifier $ orient eqs3
    else if any canDecomp eqs3
        -- decompose rule
        then calcUnifier $ decompose eqs3
    else do
        -- solve rule
        case findSolver eqs3 of
            Nothing -> Just eqs3
            (Just eq) -> calcUnifier $ solve eqs3 eq
    


-- check Fail2, Fail3 (there are no different constuctors, so no Fail1)
-- num = a -> b fails
-- a -> b = num fails
failCheck :: TypeEquation -> Maybe TypeEquation
failCheck (TNum, TComb _) = Nothing
failCheck (TComb _, TNum) = Nothing
failCheck eq = Just eq

-- check if an equation has a var on the left that appears in a type on the right
occursCheck :: TypeEquation -> Maybe TypeEquation
occursCheck (TVar x, TComb list) = 
    if appearsIn x (TComb list) 
        then Nothing 
    else Just (TVar x, TComb list)
occursCheck eq = Just eq 

-- check if rule 'orient' can be applied to an equation
canOrient :: TypeEquation -> Bool
canOrient (TNum, TVar x) = True
canOrient (TComb l, TVar x) = True
canOrient _ = False

-- check if rule 'decompose' an be applied to an equation
canDecomp :: TypeEquation -> Bool
canDecomp (TComb l1, TComb l2) = length l1 >= 2 && length l2 >=2
canDecomp _ = False


-- apply 'orient' to a list of equations
orient :: [TypeEquation] -> [TypeEquation]
orient = map orientEq
    where
        orientEq :: TypeEquation -> TypeEquation
        orientEq (TNum, TVar x) = (TVar x, TNum)
        orientEq (TComb l, TVar x) = (TVar x, TComb l) 
        orientEq other = other

-- apply 'decompose' to a list of equations
decompose :: [TypeEquation] -> [TypeEquation]
decompose = concatMap decompEq
    where
        decompEq :: TypeEquation -> [TypeEquation]
        decompEq (TComb (x1:x2:xs), TComb (y1:y2:ys)) = (x1,y1): decompEq (TComb (x2:xs), TComb (y2:ys))
        decompEq other = [other]



-- check if there is an equation of pattern (var, _), where var either appears
-- twice on the left side, or at least once on the right side in the list of all type equations.
-- returns a type equation that satisfies that condition
findSolver :: [TypeEquation] -> Maybe TypeEquation
findSolver list = find (\(lhs, rhs) -> canSolve lhs list) list
    where 
        canSolve :: Type -> [TypeEquation] -> Bool
        canSolve (TVar s) eqs = countLeftSide s eqs >= 2 || any (appearsIn s . snd) eqs
        canSolve _ _ = False

        countLeftSide :: String -> [TypeEquation] -> Int
        countLeftSide s eqs = length $ filter (appearsIn s) (map fst eqs)


-- solve takes a list of equations as input and a single equation (a, b), and replaces
-- all occurrences of a in the list of equations with b.
-- the equation itself is also contained in list. Thus, one occurrence (a, b) will become (b, b), 
-- which is later filtered with the elim rule.
-- the original equation (a,b) is then added again to the list
solve :: [TypeEquation] -> TypeEquation -> [TypeEquation]
solve list eq = eq : map (replaceWithEq eq) list
    where 
        replaceWithEq :: TypeEquation -> TypeEquation -> TypeEquation 
        replaceWithEq eq (lhs, rhs) = (replaceWith eq lhs, replaceWith eq rhs)

        replaceWith :: TypeEquation -> Type -> Type
        replaceWith (a,b) (TComb list) | a == TComb list = b 
                                       | otherwise = TComb $ map (replaceWith (a,b)) list
        replaceWith (a,b) structure    | a == structure = b
                                       | otherwise = structure


-- helper functions
-- check if a type variable (string) appears in a type
appearsIn :: String -> Type -> Bool
appearsIn s TNum         = False
appearsIn s (TVar x)     = s == x
appearsIn s (TComb list) = any (appearsIn s) list

-- simplifyType: flatten a type as far as possible
simplifyType :: Type -> Type
simplifyType TNum = TNum
simplifyType (TVar s) = TVar s
simplifyType (TComb [el]) = simplifyType el
simplifyType (TComb list) = TComb $ map simplifyType list


