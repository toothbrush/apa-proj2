-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.HM.Parser
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'Parser' for a simple, implicitly typed functional language.
--
-------------------------------------------------------------------------------

module APA2.Parser (
    -- * Parser
    parser    -- :: Component String Tm
) where

import APA2.AG
import APA2.Lexer                    (Token, lexer, keyword, var, spec)
import CCO.Component                   (Component)
import qualified CCO.Component as C    (parser)
import CCO.Parsing
import Control.Applicative

-------------------------------------------------------------------------------
-- Token parsers
-------------------------------------------------------------------------------

-- | Type of 'Parser's that consume symbols described by 'Token's.
type TokenParser = Parser Token

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

-- A 'Component' for parsing terms.
parser :: Component String Tm
parser = C.parser lexer (pTm <* eof)

-- | Parses a 'Tm'.
pTm :: TokenParser Tm
pTm = Lam <$> (spec '\\' *> var) <* spec '.' <*> pTm
  <|> foldl1 App <$> some
   (   Var <$> var
   <|>
       Let <$> (keyword "let" *> var) <* spec '=' <*> pTm <* keyword "in"
           <*> pTm <* keyword "ni"
   <|> spec '(' *> pTm <* spec ')'
   )
