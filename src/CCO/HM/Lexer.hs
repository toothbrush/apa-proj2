-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.HM.Lexer
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'Lexer' for a simple, implicitly typed functional language.
--
-------------------------------------------------------------------------------

module CCO.HM.Lexer (
    -- * Tokens
    Token      -- abstract, instance: Symbol

    -- * Lexer
  , lexer      -- :: Lexer Token

    -- * Token parser
  , keyword    -- :: String -> Parser Token String
  , var        -- :: Parser Token String
  , spec       -- :: Char -> Parser Token Char
) where

import CCO.HM.Base    (Var)
import CCO.Lexing hiding (satisfy)
import CCO.Parsing    (Symbol (describe), Parser, satisfy, (<!>))
import Control.Applicative

-------------------------------------------------------------------------------
-- Tokens
-------------------------------------------------------------------------------

-- | Type of tokens.
data Token
  = Keyword  { fromKeyword :: String }    -- ^ Keyword.
  | Var      { fromVar     :: Var    }    -- ^ Variable.
  | Spec     { fromSpec    :: Char   }    -- ^ Special character.

instance Symbol Token where
  describe (Keyword _)  lexeme = "keyword "  ++ lexeme
  describe (Var _)      lexeme = "variable " ++ lexeme
  describe (Spec _)     lexeme =                lexeme

-- | Retrieves whether a 'Token' is a 'Keyword'.
isKeyword :: Token -> Bool
isKeyword (Keyword _) = True
isKeyword _           = False

-- | Retrieves whether a 'Token' is a 'Var'.
isVar :: Token -> Bool
isVar (Var _) = True
isVar _       = False

-- | Retrieves whether a 'Token' is a 'Spec'.
isSpec :: Token -> Bool
isSpec (Spec _) = True
isSpec _        = False

-------------------------------------------------------------------------------
-- Lexer
-------------------------------------------------------------------------------

-- | A 'Lexer' that recognises (and ignores) whitespace.
layout_ :: Lexer Token
layout_ = ignore (some (anyCharFrom " \n\t"))

-- | A 'Lexer' that recognises 'Keyword' tokens.
keyword_ :: Lexer Token
keyword_ = fmap Keyword $ string "in" <|> string "let" <|> string "ni"

-- | A 'Lexer' that recognises 'Var' tokens.
var_ :: Lexer Token
var_ = Var <$> some (alphaNum <|> char '_')

-- | A 'Lexer' that recognises 'Spec' tokens.
spec_ :: Lexer Token
spec_ = Spec <$> anyCharFrom "()=\\."

-- | The 'Lexer' for the simple imperative language.
lexer :: Lexer Token
lexer = layout_ <|> keyword_ <|> var_ <|> spec_

-------------------------------------------------------------------------------
-- Token parsers
-------------------------------------------------------------------------------

-- | A 'Parser' that recognises a specified keyword.
keyword :: String -> Parser Token String
keyword key = fromKeyword <$>
              satisfy (\tok -> isKeyword tok && fromKeyword tok == key) <!>
              "keyword " ++ key

-- | A 'Parser' that recognises variables.
var :: Parser Token Var
var = fromVar <$> satisfy isVar <!> "variable"

-- | A 'Parser' that recognises a specified special character.
spec :: Char -> Parser Token Char
spec c = fromSpec <$>
         satisfy (\tok -> isSpec tok && fromSpec tok == c) <!>
         [c]