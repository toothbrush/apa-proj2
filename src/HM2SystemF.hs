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
import CCO.HM.Base (Tm)
import CCO.HM2SystemF (doConversion)
import qualified CCO.SystemF.Base as SF (Tm (Var))
import CCO.Tree (ATerm, toTree, parser, fromTree)
import Control.Arrow (arr, (>>>))

-- | The entry point for the hm2systemf program. Built up 
-- as a pipeline of functions. 
main :: IO ()
main = ioWrap $
        parser >>>
        (component toTree :: Component ATerm Tm) >>>
        convertAndType >>>
        (arr fromTree :: Component SF.Tm ATerm) >>>
        printer

-- | Calls the 'doConversion' function, which runs the 
-- AG code on the parsed Hindley-Milner term. Returns 
-- a type-annotated System F term.
convertAndType :: Component Tm SF.Tm
convertAndType = component (return . doConversion)

