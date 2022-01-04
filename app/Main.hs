module Main where

-- Hutton's Razor
-- recursive data structures
data Expr =
    Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add left right) = (eval left) + (eval right)

print_ :: Expr -> String
print_ (Lit x) = show x
print_ (Add left right) = print_ left ++ "+" ++ print_ right

main :: IO ()
main = undefined

