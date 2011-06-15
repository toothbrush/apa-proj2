--------------------------------------------------------------------------------
--
-- Simple pipeline which calls the AG code. 
-- Input:  parsed Hindley-Milner term,
-- Output: type-annotated System F term,
-- Both in ATerm format (portable text format, useful
-- for pipelining functions in the terminal).
--
-- Author : Paul van der Walt <paul@denknerd.nl>
--
--------------------------------------------------------------------------------
import CCO.Component (Component, component, printer, ioWrap)
import CCO.Tree (ATerm, toTree, parser, fromTree)
import Control.Arrow (arr, (>>>))

import Base (
    Term (..),
    inferredType_Syn_Term,
    wrap_Term,
    sem_Term,
    substitution_Syn_Term,
    Inh_Term (..),
    annotated_Syn_Term,
    generalise
    )
import qualified Infer.Typed as SF (Tm (..))

import TreeInstances
import Debug.Trace


-- | The entry point for the hm2systemf program. Built up 
-- as a pipeline of functions. 
main :: IO ()
main = ioWrap $
        parser >>>
        (component toTree :: Component ATerm Term) >>>
        convertAndType >>>
        (arr fromTree :: Component SF.Tm ATerm) >>>
        printer

-- | Calls the 'doConversion' function, which runs the 
-- AG code on the parsed Hindley-Milner term. Returns 
-- a type-annotated System F term.
convertAndType :: Component Term SF.Tm
convertAndType = component (return . doConversion)

doConversion :: Term -> SF.Tm
doConversion t = let inferredType = inferredType_Syn_Term (wrap_Term (sem_Term t) inherit)
                     annotated    = annotated_Syn_Term (wrap_Term (sem_Term t) inherit)
                     -- we must generalise one more time, to figure out which
                     -- TyLam's must be prepended. 
                     (ty', coercion) = generalise [] inferredType
                 in  trace ("Type of entire expression: \n  :: "++show ty'++"\n") 
                     (coercion annotated)

-- | The top-level inherited attribute to be passed to an attribute grammar
-- for System F. In our case, we want to start with an empty type
-- environment, and a variable counter of 0 (used for generating fresh
-- type variables).
inherit :: Inh_Term
inherit = Inh_Term { typeEnvironment_Inh_Term = [] -- Start with empty environment.
                 , counter_Inh_Term = 0          -- Start variable-name-seed at 0.
                 }

