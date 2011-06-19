module Components where
import Language.Haskell.Exts.Parser
import qualified Language.Haskell.Exts.Syntax as H
import qualified Data.Map as DM
import APA2.AG
import Data.Generics.Schemes
import Data.Generics.Aliases
import Debug.Trace

inferTypes :: MH -> Ty
inferTypes tm =
  let inheritAttr = Inh_MH { typeEnvironment_Inh_MH = DM.empty
                           , counter_Inh_MH = 0
                           }
  in algW_Syn_MH (wrap_MH (sem_MH tm) inheritAttr)

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

  hExpr (H.Let (H.BDecls ((H.FunBind ((H.Match _ (H.Ident x) pts _ (H.UnGuardedRhs e) _) : _)) : _)) e')
    | isRecursiveLet x e  = LetRec x (hExpr e) (toLambda (hExpr e') pts)
    | otherwise           = Let x (hExpr e) (toLambda (hExpr e') pts)

  hExpr (H.Let (H.BDecls ((H.PatBind _ (H.PVar (H.Ident x)) Nothing (H.UnGuardedRhs e) _) : _)) e')
    | isRecursiveLet x e = LetRec x (hExpr e) (hExpr e')
    | otherwise          = Let x (hExpr e) (hExpr e')

  hExpr (H.If c e e') = If (hExpr c) (hExpr e) (hExpr e')

  hExpr (H.Lambda _ pt e) = toLambda (hExpr e) pt

  hExpr (H.Paren e) = hExpr e

  hExpr (H.InfixApp e (H.QConOp (H.Special H.Cons)) (H.List [])) = Cons (hExpr e) Nil
  hExpr (H.InfixApp e (H.QConOp (H.Special H.Cons)) e') = Cons (hExpr e) (hExpr e')

  hPat (H.PVar n) = hName n

  hQName (H.UnQual (H.Ident x)) = x

  hName (H.Ident x) = x

  toLambda = foldr (\(H.PVar n) -> Lambda $ hName n)

  isRecursiveLet :: String -> H.Exp -> Bool
  isRecursiveLet f e =
    (not . null) $ listify cond (removeShadowingExpressions f e)
    where
      cond (H.Ident x) = x == f
      cond _           = False

  removeShadowingExpressions :: String -> H.Exp -> H.Exp
  removeShadowingExpressions f = everywhere' (mkT letOrLam)
    where
    cond x f l
      | x == f    = emptyExp
      | otherwise = l

    letOrLam l@(H.Let (H.BDecls ((H.FunBind ((H.Match _ (H.Ident x) _ _ _ _) :_ )) : _)) _) = cond x f l
    letOrLam l@(H.Let (H.BDecls ((H.PatBind _ (H.PVar (H.Ident x)) _ _ _) : _)) _)          = cond x f l

    letOrLam l@(H.Lambda _ pt _) =
      case [f | H.PVar n <- pt, hName n == f] of
        []  -> l
        _   -> emptyExp

    letOrLam l = l

  emptyExp = H.Var (H.UnQual $ H.Ident "$%")
