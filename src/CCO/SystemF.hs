-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.SystemF
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- System F.
--
-------------------------------------------------------------------------------

module CCO.SystemF (
    -- * Syntax
    TyVar                               -- = String
  , Var                                 -- = String
  , Ty (TyVar, Arr, Forall)             -- instances: Tree
  , Tm (Var, Lam, App, TyLam, TyApp)    -- instances: Tree
) where

import CCO.SystemF.Base    (TyVar, Var, Ty (..), Tm (..))