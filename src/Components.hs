module Components where
import Language.Haskell.Exts.Parser
import qualified Language.Haskell.Exts.Syntax as H
import qualified Data.Map as DM
import APA2.AG

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

  hExpr (H.Let (H.BDecls (d:_)) e) = hDecl d (hExpr e)

  hExpr (H.If c e e') = If (hExpr c) (hExpr e) (hExpr e')

  hExpr (H.Lambda _ pt e) = toLambda (hExpr e) pt

  hExpr (H.Paren e) = hExpr e

  hExpr (H.InfixApp e (H.QConOp (H.Special H.Cons)) (H.List [])) = Cons (hExpr e) Nil
  hExpr (H.InfixApp e (H.QConOp (H.Special H.Cons)) e') = Cons (hExpr e) (hExpr e')

  hPat (H.PVar n) = hName n

  hDecl (H.PatBind _ (H.PVar (H.Ident x)) Nothing (H.UnGuardedRhs e) _) = Let x (hExpr e)
  hDecl (H.FunBind (m:_)) = hMatch m

  hMatch (H.Match _ n pts nothing (H.UnGuardedRhs e) _) = Let (hName n) (toLambda (hExpr e) pts)

  hQName (H.UnQual (H.Ident x)) = x

  hName (H.Ident x) = x

  toLambda = foldr (\(H.PVar n) -> Lambda $ hName n)

