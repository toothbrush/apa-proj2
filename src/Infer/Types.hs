{-# LANGUAGE StandaloneDeriving,
             TypeSynonymInstances #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Types
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  Paul van der Walt <paul@denknerd.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module contains some types which are useful for Algorithm W. 
-- A number of helper functions are also defined here. 
--
-------------------------------------------------------------------------------
module Infer.Types where
    import qualified Infer.Typed as SF
    import Data.List

    import Debug.Trace
    
    -- | A Type environment maps variable names to types.
    type TyEnv = [(SF.Var,SF.Ty)]
    -- | TyVar and Var are just strings (variable names).
    type TyVar = String
    type Var   = String
    
    -- | A type substitution. Used for substitution of type variables
    -- in signatures. 
    data TySubst = Identity -- ^ Identity substitution, do nothing. 
                 | Sub SF.TyVar SF.Ty -- ^ Substitute a type variable with a type. 
                 | Dot TySubst TySubst -- ^ Chain substitutions together.
                 deriving Show

    -- | Useful for printing the type of the entire equation.
    instance Show SF.Ty where 
        show (SF.TyVar x) = x
        show (SF.Arr t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
        show (SF.Forall tv ty) = "(forall " ++ tv ++ ". " ++ show ty ++ ")"

    -- This was nice for debugging.
    deriving instance Show SF.Tm

    -- | A class which is useful for defining functions such as 'applySubst' (which
    -- substitutes all occurences of a variable with a type, and 'ftv' which returns
    -- all the free type variables in the argument.
    class Types a where
        applySubst :: TySubst -> a -> a
        ftv        :: a -> [Var]

    instance Types SF.Ty where
        -- The free type variables are accumulated in the same way
        -- as recommended in the course slides.
        ftv (SF.TyVar tv)                  = [tv]
        ftv (SF.Arr t1 t2)                 = nub (ftv t1 ++ ftv t2)
        ftv (SF.Forall tv ts)              = ftv ts \\ [tv]
        -- The implementation of applySubst is exactly like presented in the
        -- slides of this course. 
        applySubst Identity t           = t --identity
        applySubst (Dot s1 s2) t        = applySubst s1 (applySubst s2 t)
        applySubst (Sub a t0) (SF.TyVar t) = if a == t
                                               then t0
                                               else SF.TyVar t --identity
        applySubst s@(Sub a t0) (SF.Arr t1 t2) = SF.Arr
                                                  (applySubst s t1)
                                                  (applySubst s t2)
        applySubst s@(Sub a t0) (SF.Forall tv ts) = if a == tv
                                                      then SF.Forall tv ts --identity
                                                      else SF.Forall tv (applySubst s ts)

    instance Types TyEnv where
        -- Simply extend the functions to support lists
        -- of whatever they already support.
        applySubst _ []         = []
        applySubst s ((v,ts):r) = (v, applySubst s ts):applySubst s r
        ftv []         = []
        ftv ((v,ts):r) = nub $ ftv ts ++ ftv r

    -- | The unification algorithm. If none of the cases match, fail.
    unify :: SF.Ty -> SF.Ty -> TySubst
    unify t1@(SF.TyVar tv1) t2@(SF.TyVar tv2) | tv1 == tv2 = Identity
                                        | tv1 `notElem` ftv t2 = Sub tv1 t2
                                        | tv2 `notElem` ftv t1 = Sub tv2 t1
                                        | otherwise = error "Cannot unify. Error."
    unify (SF.TyVar tv1) t | tv1 `notElem` ftv t = Sub tv1 t
                        | otherwise = error $ "Occurs check: " ++
                                                show tv1 ++ " = " ++
                                                show t
                                                ++ "\nCannot create infinite type."
    unify t (SF.TyVar tv2) | tv2 `notElem` ftv t = Sub tv2 t
                        | otherwise = error $ "Occurs check: " ++ 
                                                show tv2 ++ " = " ++
                                                show t
                                                ++ "\nCannot create infinite type."
    unify (SF.Arr t11 t12) (SF.Arr t21 t22) = let theta1 = unify t11 t21
                                                  theta2 = unify
                                                              (applySubst theta1 t12)
                                                              (applySubst theta1 t22)
                                              in Dot theta2 theta1
    unify t1 t2 = error $ "Unification failure. \nt_1 = " ++ 
                          show t1 ++ "\nt_2 = " ++ 
                          show t2
