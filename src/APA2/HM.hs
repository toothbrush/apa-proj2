-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.HM
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A simple, implicitly typed functional language.
--
-------------------------------------------------------------------------------

module APA2.HM (
    -- * Syntax
    Var                         -- = String
  , Tm (Var, Lam, App, Let)    -- instances: Tree

    -- * Parser
  , parser                      -- :: Component String Tm
) where

import APA2.AG
import APA2.Parser    (parser)
