module Parser.PrettyPrint where

import Base

instance Show (Term) where
    show (Var s)     = "$"++s
    show (Lam s x)   = "\\ $"++s++" -> "++show x
    show (App t1 t2) = "("++show t1 ++ ") ("++show t2 ++ ")"
    show (Let x t i) = "let " ++ x ++ "="++show t++ " in "++ show i ++ " ni"
