{-
Contains the lexer function and the 'Token' data type.
lexer function is inspired by the example in the happy parser documentation.
-}

module Lexer ( lexer, Token(..) ) where


import Control.Monad.Trans.Except
import Definitions( MyError(..), MyException )
import Data.Char ( isSpace, isDigit, isAlpha )


data Token
         = TokenEq
         | TokenAdd
         | TokenSub
         | TokenMul
         | TokenDiv
         | TokenPow
         | TokenLB
         | TokenRB
         | TokenComma
         | TokenNum Double
         | TokenId String
         | TokenLam
         | TokenArr
         deriving(Show)



{-
lexer is a function that takes a String and outputs a list of Tokens.
this is the preprocessing step of parsing.
lexer will return an error if an invalid character occurs.
-}
lexer :: (Monad m) => String -> MyException m [Token]
lexer [] = return []
lexer ('-':'>':xs) = (TokenArr :) <$> lexer xs
lexer ('-':xs) = (TokenSub :) <$> lexer xs
lexer ('(':xs) = (TokenLB :)  <$> lexer xs
lexer (')':xs) = (TokenRB :)  <$> lexer xs
lexer ('=':xs) = (TokenEq :)  <$> lexer xs
lexer ('+':xs) = (TokenAdd :) <$> lexer xs
lexer ('*':xs) = (TokenMul :) <$> lexer xs
lexer ('/':xs) = (TokenDiv :) <$> lexer xs
lexer ('^':xs) = (TokenPow :) <$> lexer xs
lexer (',':xs) = (TokenComma :) <$> lexer xs
lexer ('\\':xs) = (TokenLam :) <$> lexer xs
lexer ('.':cs) = lexNum ('0':'.':cs)
lexer (c : cs)    
      | isSpace c = lexer cs
      | isIdStarter c = lexId (c:cs)
      | isDigit c = lexNum (c:cs)
      | otherwise = throwE $ ParseError $ "Invalid character " ++ [c]


-- helper functions

-- consume all characters that can be part of an id, then call lexer on the rest
lexId :: (Monad m) => String -> MyException m [Token]
lexId [] = return []
lexId cs = let (str, rest) = span isIdPart cs 
             in (TokenId str :) <$> lexer rest 
    where
        -- variables start with letter, _, ', but can contain numbers as well
        isIdPart :: Char -> Bool
        isIdPart c = isIdStarter c || isDigit c

-- variables can start with letters, _ or '
isIdStarter :: Char -> Bool
isIdStarter c = isAlpha c || c `elem` "_'"

-- consume all chars that can be part of a num.
-- nums can also have the special forms
-- .5 -> 0.5
-- 5. -> 5
-- 1.02342 -> 1.02342
lexNum :: (Monad m) => String -> MyException m [Token]
lexNum [] = return []
lexNum cs = 
    let (ld, rest) = span isDigit cs 
     in case rest of
        []       -> return [TokenNum (read ld)]
        ['.']    -> return [TokenNum (read ld)]
        ('.':xs) -> let (decs, rest2) = span isDigit xs in
                          if decs == "" 
                          then (TokenNum (read ld) :) <$> lexer rest2 
                          else (TokenNum (read (ld ++ "." ++ decs)) :) <$> lexer rest2
        _        -> (TokenNum (read ld) :) <$> lexer rest


