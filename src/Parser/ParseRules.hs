{-# LANGUAGE FlexibleContexts #-}
module Parser.ParseRules where
 
    import Text.ParserCombinators.UU
    import Text.ParserCombinators.UU.Utils
    import Text.ParserCombinators.UU.BasicInstances
    import Parser.Base

    pTerm :: Parser Term
    pTerm = (
             pVar 
         <|> pLam
         <|> pApp
         <|> pLet
            ) 
          <* pMunch ws

    pVarName :: Parser String
    pVarName = pToken "$" *> pMunch (\x -> elem x (['a'..'z']++['0'..'9']))

    -- | ws tells us if a char is whitespace
    ws :: Char -> Bool
    ws x = elem x "\n\r\t "

    pVar :: Parser Term
    pVar = Var <$> pVarName <* pMunch ws

    pLam :: Parser Term
    pLam = Lam <$ keyword "\\" <*> pVarName <* keyword "->" 
               <*> pTerm <* pMunch ws

    pApp :: Parser Term
    pApp = App <$> parens pTerm <*> parens pTerm

    pLet :: Parser Term
    pLet = Let <$ keyword "let" <*> pVarName <* keyword "=" 
               *> pTerm <* keyword "in" *> pTerm <* keyword "ni"

    keyword w = pMunch ws *> pToken w <* pMunch ws

    parens p = pSymbol "(" *> p <* pSymbol ")"
