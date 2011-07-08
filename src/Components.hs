{-# LANGUAGE TypeSynonymInstances #-}
module Components where
<<<<<<< HEAD

import            Language.Haskell.Exts.Parser
import qualified  Language.Haskell.Exts.Syntax as H
import            Data.Map (Map)
import qualified  Data.Map as DM
import qualified  Data.Set as DS
=======
import Language.Haskell.Exts.Parser
import qualified Language.Haskell.Exts.Syntax as H
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as DM
import qualified Data.Set as DS
>>>>>>> feb4338a27ce92a55ea205a3e169a2bcbdf2c705

import APA2.AG
import Data.Generics.Schemes
import Data.Generics.Aliases

-- | Setup the inherited attributes for the AG
initialInheritedAttributes :: Inh_MH
initialInheritedAttributes =
  Inh_MH { typeEnvironment_Inh_MH = DM.empty
         , annEnvironment_Inh_MH = DM.empty
         , counter_Inh_MH = 0
         , matchTy_Inh_MH = undefined -- suppress warning. only used in caseBlock
         }

-- | Execute the annotated Algorithm W on the provided MH AST
w :: MH -> (Ty, SAnn, SimpleSubstitution, Constraints, Expressions, String, Map String MH)
w tm = let wrappedDS = wrap_MH (sem_MH tm) initialInheritedAttributes
       in  ( ty_Syn_MH           wrappedDS
           , annotation_Syn_MH   wrappedDS
           , substitution_Syn_MH wrappedDS
           , constraints_Syn_MH  wrappedDS
           , expressions_Syn_MH  wrappedDS
           , debug_Syn_MH        wrappedDS
           , annotDict_Syn_MH    wrappedDS
           )

<<<<<<< HEAD
-- | Grabs the constraints from the resultt
getConstraints :: (t, t1, t2, t3, t4, t5, t6) -> t3
=======
getConstraints :: (a,b,c,d,e,f,g) -> d
>>>>>>> feb4338a27ce92a55ea205a3e169a2bcbdf2c705
getConstraints (_,_,_,c,_,_,_) = c

-- | Accepts a filename and generates debug output for it containing analysis results.
debugFile :: FilePath -> IO ()
debugFile fl = do
  cnts <- readFile fl
  debugInference . parseProgram $ cnts

-- | Output detailed information from the inference process for debug purposes.
debugInference :: MH -> IO ()
debugInference tm =
  do
    let (ty, annotation, subst, constraints, exprs, debug, annots) = w tm
    putStrLn ("Program: \n    " ++ show tm)
    print tm 
    putStrLn ""
    putStrLn "Substitution:"
    print subst
    putStrLn ""
    putStrLn "Ty:"
    print ty
    putStrLn ""
    putStrLn "Top level annotation: "
    print annotation
    let ppoint = DM.findWithDefault (AnnVar "the empty set") (fromSAnn annotation) (worklist constraints)
    putStr $ "... which maps to: " ++ show ppoint ++ "\n"
    putStr $ DS.fold (\x->(++) (
        x ++ " is in fact \"" ++ show (annots DM.! x) ++ "\"\n"
        ) ) ""
        (toSet ppoint)
    putStrLn "Annotation dictionary:"
    putStrLn (ppMap annots)
    putStrLn ""
    putStrLn "Constraints: "
    print (DS.toList constraints)
    putStrLn "\nNew_Constraints: "
    printAnalysis (worklist constraints)
    putStrLn ""
    putStrLn "Expressions: "
    printExpressions (applySubst subst exprs)
    putStrLn ""
    putStrLn debug

-- | Unpacks SAnn datatype and returns an AnnVar string
fromSAnn :: SAnn -> AnnVar
fromSAnn (AnnVar v) = v
fromSAnn _          = error "danger will robinson"

-- | Unpacks an AnnSet to get the inner set
toSet :: SAnn -> DS.Set Point
toSet (AnnSet x) = x
toSet _          = DS.empty

-- | Prints an analysis result
analysisResult :: MH -> IO ()
analysisResult tm = 
  do 
    let (_, _, _, constraints, exprs, _, _) = w tm
    putStrLn "Analysis result: " 
    printExpressions (applySubst (solutionSubst constraints) exprs)

-- | Turns a map of constraints into a substitution
solutionSubst :: Constraints -> SimpleSubstitution
solutionSubst cs = 
  DM.foldrWithKey (\var result next -> Dot (AnnSub var result) next) Identity (worklist cs)

-- | Converts a list of expressions to a String
printExpressions' :: (Show a1, Show a) => [(a1, a)] -> String
printExpressions' exprs = foldr (\(ty,e) acc -> show e ++ " : " ++ show ty ++ acc) "" exprs

-- | Prints a list of expressions
printExpressions :: (Show a1, Show a) => [(a1, a)] -> IO ()
printExpressions exprs = mapM_ (\(a,e) -> putStrLn (show e ++ " : " ++ show a)) exprs

-- | Takes a string containing Haskell code and parses it.
parseProgram :: String -> MH
parseProgram = translate . fromParseResult . parseExp 

-- | Takes a haskell-src-exts AST and converts it to our simple lambda-calculus syntax.
translate :: H.Exp -> MH
translate = hExpr
  where
  hExpr (H.Lit (H.Int i)) = VInt i
  hExpr (H.Con (H.UnQual (H.Ident x))) | x == "True"  = VBool True
                                       | x == "False" = VBool False

  hExpr (H.Var n) = Var (hQName n)

  hExpr (H.App e e') = App (hExpr e) (hExpr e')

  hExpr (H.Let (H.BDecls (H.FunBind (H.Match _ (H.Ident f) pts _ (H.UnGuardedRhs e) _ : _) : _)) e')
    | isRecursiveLet f e  = let (Lambda x b) = toLambda (hExpr e) pts
                            in LetRec f (Just x) b (hExpr e')
    | otherwise           = Let f (toLambda (hExpr e) pts) (hExpr e')

  hExpr (H.Let (H.BDecls (H.PatBind _ (H.PVar (H.Ident f)) Nothing (H.UnGuardedRhs e) _ : _)) e')
    | isRecursiveLet f e = case hExpr e of
                            (Lambda x b) -> LetRec f (Just x) b (hExpr e')
                            b            -> LetRec f Nothing b (hExpr e')
    | otherwise          = Let f (hExpr e) (hExpr e')

  hExpr (H.If c e e') = If (hExpr c) (hExpr e) (hExpr e')

  hExpr (H.Lambda _ pt e) = toLambda (hExpr e) pt

  hExpr (H.Paren e) = hExpr e

  hExpr (H.List ls) = foldr (Cons . hExpr) Nil ls
  hExpr (H.InfixApp e (H.QConOp (H.Special H.Cons)) (H.List [])) = Cons (hExpr e) Nil
  hExpr (H.InfixApp e (H.QConOp (H.Special H.Cons)) e') = Cons (hExpr e) (hExpr e')
  hExpr (H.InfixApp e (H.QVarOp (H.UnQual (H.Symbol op))) e') = Op op (hExpr e) (hExpr e') 
  hExpr (H.Case e (c1:c2:[])) = CaseBlck (hExpr e) (mkCaseBlck c1) (mkCaseBlck c2)

  hExpr e = notSupported e

  mkCaseBlck (H.Alt _ pat (H.UnGuardedAlt ex) _) = CaseAlt (mkPat pat) (hExpr ex)
  mkCaseBlck _ = undefined -- TODO: Nice error
  mkPat (H.PLit (H.Int n)) = VInt n
  mkPat (H.PVar (H.Ident n)) = Var n
  mkPat (H.PParen p) = mkPat p
  mkPat (H.PList ps) = foldr (Cons . mkPat) Nil ps
  mkPat (H.PInfixApp p (H.Special H.Cons) (H.PList [])) = Cons (mkPat p) Nil
  mkPat (H.PInfixApp p (H.Special H.Cons) ps) = Cons (mkPat p) (mkPat ps)
  mkPat (H.PApp (H.UnQual (H.Ident n)) _) = case n of
                                                "True"  -> VBool True
                                                "False" -> VBool False
                                                val     -> Var val
  mkPat e = notSupported e

  hQName (H.UnQual (H.Ident x)) = x
  hQName e = notSupported e

  hName (H.Ident x) = x
  hName e           = notSupported e

  toLambda = foldr o
    where o (H.PVar n) = Lambda $ hName n
          o _          = id

  isRecursiveLet :: String -> H.Exp -> Bool
  isRecursiveLet f e =
    (not . null) $ listify cond (removeShadowingExpressions f e)
    where
      cond (H.Ident x) = x == f
      cond _           = False

  removeShadowingExpressions :: String -> H.Exp -> H.Exp
  removeShadowingExpressions f = everywhere' (mkT letOrLam)
    where
    cond x l
      | x == f    = emptyExp
      | otherwise = l

    letOrLam l@(H.Let (H.BDecls (H.FunBind (H.Match _ (H.Ident x) _ _ _ _ :_ ) : _)) _) = cond x l
    letOrLam l@(H.Let (H.BDecls (H.PatBind _ (H.PVar (H.Ident x)) _ _ _ : _)) _)        = cond x l

    letOrLam l@(H.Lambda _ pt _) =
      case [f | H.PVar n <- pt, hName n == f] of
        []  -> l
        _   -> emptyExp

    letOrLam l = l

  notSupported e = error ("Expression not supported: " ++ show e)

  emptyExp = H.Var (H.UnQual $ H.Ident "$%")

