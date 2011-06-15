{-# LANGUAGE FlexibleContexts #-}
module Parser.ParseRules where
 
    import Text.ParserCombinators.UU
    import Text.ParserCombinators.UU.Utils
    import Text.ParserCombinators.UU.BasicInstances
    import Parser.Base

    pTerm :: Parser Term
    pTerm =  (pVar 
         <|> pLam)
       --  <|> pApp
       --  <|> pLet
          <* pMaybe (pLF)

    pVarName :: Parser String
    pVarName = pToken "$" *> pMunch (\x -> elem x (['a'..'z']++['0'..'9']))

    pVar :: Parser Term
    pVar = Var <$> pVarName

    pLam :: Parser Term
    pLam = Lam <$ pToken "\\ " <*> pVarName <* pToken "->" <*> pTerm
    pApp = undefined
    pLet = undefined
