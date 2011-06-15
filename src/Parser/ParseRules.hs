{-# LANGUAGE FlexibleContexts #-}
module Parser.ParseRules where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances
import Parser.Base

pTerm :: Parser Term
pTerm =
         pVar
     <|> pLam
     <|> pApp
     <|> pLet

pVarName :: Parser String
pVarName = lexeme (pToken "$" *> pList (pLetter <|> pDigit))

pVar :: Parser Term
pVar = Var <$> pVarName

pLam :: Parser Term
pLam = Lam <$> (pSymbol "\\" *> pVarName) <*> (pSymbol "->" *> pTerm)

pApp :: Parser Term
pApp = App <$> pParens pTerm <*> pParens pTerm

pLet :: Parser Term
pLet = Let <$> (pSymbol "let" *> pVarName <* pSymbol "=")
           <*> pTerm <*> (pSymbol "in" *> pTerm <* pSymbol "ni")
