{-# LANGUAGE StandaloneDeriving #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.HM2SystemF
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  Paul van der Walt <paul@denknerd.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- HM type inferencer. This function returns a type-annotated System F term,
-- given an implicitly-typed HM term. Calls the AG defined in Infer.ag. 
--
-------------------------------------------------------------------------------

module CCO.HM2SystemF (
    doConversion
) where

import CCO.HM.AG (
    Tm (..), Tm_ (..),
    inferredType_Syn_Tm,
    wrap_Tm,
    sem_Tm,
    substitution_Syn_Tm,
    Inh_Tm (..),
    annotated_Syn_Tm,
    generalise
    )
import qualified CCO.SystemF.AG as SF (Tm (..))
import CCO.Types

import Debug.Trace

doConversion :: Tm -> SF.Tm
doConversion t = let inferredType = inferredType_Syn_Tm (wrap_Tm (sem_Tm t) inherit)
                     annotated    = annotated_Syn_Tm (wrap_Tm (sem_Tm t) inherit)
                     -- we must generalise one more time, to figure out which
                     -- TyLam's must be prepended. 
                     (ty', coercion) = generalise [] inferredType
                 in  trace ("Type of entire expression: \n  :: "++show ty'++"\n") 
                     (coercion annotated)

-- | The top-level inherited attribute to be passed to an attribute grammar
-- for System F. In our case, we want to start with an empty type
-- environment, and a variable counter of 0 (used for generating fresh
-- type variables).
inherit :: Inh_Tm
inherit = Inh_Tm { typeEnvironment_Inh_Tm = [] -- Start with empty environment.
                 , counter_Inh_Tm = 0          -- Start variable-name-seed at 0.
                 }
