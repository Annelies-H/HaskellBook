module HuttonsRazor where

--
--Write the Eval function which reduces an expression to a final sum

data Expr = Lit Integer | Add Expr Expr
  -- The given data type takes the form of a tree
  
eval :: Expr -> Integer
eval (Lit n)   = n
eval (Add x y) = (+) (eval x) (eval y)

--
--Write a printer for the expressions

printExpr :: Expr -> String
printExpr (Lit n)   = show n
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y

--tests
a1 = Add (Lit 9001) (Lit 1)
a2 = Add  a1 (Lit 20001)
a3 = Add (Lit 1) a2 