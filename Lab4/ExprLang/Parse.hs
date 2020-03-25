module Parse where

import Absyn
import ExprPar 
import ExprLex

-- | Call generated expr lexer and parser on input string
-- | > ghci Parse.hs Absyn.hs ExprLex.hs ExprPar.hs

parseFromString :: String -> Expr
parseFromString s = (exprParser . exprLexer) s

parseFromFile :: String -> IO ()
parseFromFile filename = do 
    s <- readFile filename
    print $ parseFromString s 


getindex :: Eq a => [a] -> a -> Int
getindex vs x 
    = case vs of []     -> error "Variable not found"
                 (y:yr) -> if x == y then 0 else 1 + getindex yr x


scomp :: Expr -> [StackValue] -> [SInstr]
scomp (CstI i) cenv = [SCstI i]
scomp (Var x) cenv  = [SVar (getindex cenv (Bound x))]
scomp (Let x erhs ebody) cenv
    = scomp erhs cenv ++ scomp ebody ((Bound x):cenv) ++ [SSwap, SPop]
scomp (Prim op e1 e2) cenv
    = case op of 
        "+" -> scomp e1 cenv ++ scomp e2 (Value:cenv) ++ [SAdd] 
        "-" -> scomp e1 cenv ++ scomp e2 (Value:cenv) ++ [SSub] 
        "*" -> scomp e1 cenv ++ scomp e2 (Value:cenv) ++ [SMul] 
        _   -> error "scomp: unknown operator"


compString :: String -> [SInstr]
compString s = scomp (parseFromString s) []
