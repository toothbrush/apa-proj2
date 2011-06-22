module Components where
import Language.Haskell.Exts.Parser
import qualified Language.Haskell.Exts.Syntax as H
import qualified Data.Map as DM
import APA2.AG
import Data.Generics.Schemes
import Data.Generics.Aliases

initialInheritedAttributes :: Inh_MH
initialInheritedAttributes = 
  Inh_MH { typeEnvironment_Inh_MH = DM.empty
         , counter_Inh_MH = 0
         }

w :: MH -> (Ty, SimpleSubstitution, Constraint)
w tm =
  (ty_Syn_MH (wrap_MH (sem_MH tm) initialInheritedAttributes)
  ,substitution_Syn_MH (wrap_MH (sem_MH tm) initialInheritedAttributes)
  ,constraints_Syn_MH (wrap_MH (sem_MH tm) initialInheritedAttributes)
  )

inferTypes :: MH -> Ty
inferTypes tm = let (ty,_,_) = w tm
                in ty

debugFile :: FilePath -> IO ()
debugFile fl = do
  cnts <- readFile fl
  debugInference . parseProgram $ cnts

debugInference :: MH -> IO ()
debugInference tm =
  do
    let (ty, subst, constraints) = w tm
    putStrLn ("Program: " ++ show tm)
    putStrLn "Substitution:"
    print subst
    putStrLn "Ty:"
    print ty
    putStrLn "Constraints:"
    print constraints

parseProgram :: String -> MH
parseProgram = translate . fromParseResult . parseExp 

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

  hExpr (H.InfixApp e (H.QConOp (H.Special H.Cons)) (H.List [])) = Cons (hExpr e) Nil
  hExpr (H.InfixApp e (H.QConOp (H.Special H.Cons)) e') = Cons (hExpr e) (hExpr e')

  hExpr (H.InfixApp e (H.QVarOp (H.UnQual (H.Symbol op))) e') = Op op (hExpr e) (hExpr e') 

  hExpr e = notSupported e

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

