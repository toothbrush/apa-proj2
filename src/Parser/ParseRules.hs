{-# LANGUAGE FlexibleContexts #-}
module Parser.ParseRules where
 
    import Text.ParserCombinators.UU
    import Text.ParserCombinators.UU.Utils
    import Text.ParserCombinators.UU.BasicInstances
    import Parser.Base

    pTerm :: Parser Term
    pTerm = Var <$> pMunch (const True)
