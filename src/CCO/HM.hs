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

module CCO.HM (
    -- * Syntax
    Var                         -- = String
  , Tm (Var, Lam, App, Let)    -- instances: Tree

    -- * Parser
  , parser                      -- :: Component String Tm
) where

import CCO.HM.AG
import CCO.HM.Parser    (parser)
