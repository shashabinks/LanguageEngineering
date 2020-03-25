{- # Abstract syntax for the simple expression language # -}

module Absyn where 

import ExprLex

data Expr = CstI Int 
          | Var  String 
          | Let  String Expr Expr
          | If   Expr   Expr Expr
          | Prim String Expr Expr
          deriving Show

data SInstr = SCstI Int     -- push integer
            | SVar Int      -- push variable from env
            | SAdd          -- pop args, push sum
            | SSub          -- pop args, push difference
            | SMul          -- pop args, push product
            | SPop          -- pop value/unbind var
            | SSwap         -- exchange top and next

data StackValue = Value         -- a computed value
                | Bound String  -- a bound variable
                deriving Eq