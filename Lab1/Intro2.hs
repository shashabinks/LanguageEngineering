
{- Evaluating simple expressions with variables -}

module Intro2 where

import Prelude hiding (lookup)

{- Association lists map object language variables to their values -}

env      = [("a", 3), ("c", 78), ("baf", 666), ("b", 111)]

emptyenv = []

lookup :: [(String, Int)] -> String -> Int
lookup [] x         = error (x ++ " not found")
lookup ((y, v):r) x = if x == y then v else lookup r x

cvalue = lookup env "c"


{- Object language expressions with variables -}

data Expr = CstI Int 
          | Var String
          | Prim String Expr Expr  
          deriving Show
          
e1 = CstI 17
e2 = Prim "+" (CstI 3) (Var "a")
e3 = Prim "+" (Prim "*" (Var "b") (CstI 9)) (Var "a")

e4 = Prim "==" (CstI 1)(CstI 2)
e5 = Prim "max" (CstI 5)(CstI 2)
e6 = Prim "min" (CstI 12)(CstI 4)


{- Evaluation within an environment -}

eval :: Expr -> [(String, Int)] -> Int 
eval (CstI i) env   = i
eval (Var x)  env   = lookup env x
eval (Prim op e1 e2) env 
    = let i1 = eval e1 env
          i2 = eval e2 env
      in case op of
              "+" -> i1 + i2
              "*" -> i1 * i2
              "-" -> i1 - i2
              "min" -> min(eval e1 env) (eval e2 env)
              "max" -> max(eval e1 env) (eval e2 env)
              "==" -> if eval e1 env == eval e2 env then 1 else 0
               

e1v  = eval e1 env
e2v1 = eval e2 env
e2v2 = eval e2 [("a", 314)]
e3v  = eval e3 env
e4v  = eval e4 env
e5v  = eval e5 env
e6v  = eval e6 env



