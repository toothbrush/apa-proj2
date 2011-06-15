module Parser.PrettyPrint where

    import Parser.Base

    instance Show (Term) where
        show (Var s) = s
        show (_    ) = "coming soon"
