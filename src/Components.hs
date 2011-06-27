{-# LANGUAGE TypeSynonymInstances #-}
module Components where
import Language.Haskell.Exts.Parser
import qualified Language.Haskell.Exts.Syntax as H
import           Data.Map (Map)
import qualified Data.Map as DM
import           Data.Set (Set)
import qualified Data.Set as DS

import APA2.AG
import Data.Generics.Schemes
import Data.Generics.Aliases

initialInheritedAttributes :: Inh_MH
initialInheritedAttributes =
  Inh_MH { typeEnvironment_Inh_MH = DM.empty
         , counter_Inh_MH = 0
         }

w :: MH -> (Ty, SimpleSubstitution, Constraints)
w tm = let wrappedDS = wrap_MH (sem_MH tm) initialInheritedAttributes
       in  ( ty_Syn_MH           wrappedDS
           , substitution_Syn_MH wrappedDS
           , constraints_Syn_MH  wrappedDS
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

instance Lattice SAnn where
  top    = undefined
  bottom = AnnSet DS.empty
  join   = annUnion

annUnion :: SAnn -> SAnn -> SAnn
annUnion (AnnSet s)   (AnnSet s') = AnnSet (DS.union s s')
annUnion a@(AnnVar v) a' 
  | containsAnnVar v a' = a' 
  | otherwise           = AnnUnion a a'
annUnion a@(AnnUnion _ _) a'@(AnnVar v)
  | containsAnnVar v a = a 
  | otherwise          = AnnUnion a a'
annUnion b@(AnnUnion a a') c@(AnnSet _) = AnnUnion b c
annUnion a@(AnnUnion _ _) b@(AnnUnion _ _) = mkDisjoint a b

-- maybe monad?
mkDisjoint :: SAnn -> SAnn -> SAnn 
mkDisjoint c@(AnnUnion a a') d@(AnnUnion b b') = 
  let vars = collectAnnVars a `DS.union` collectAnnVars a'

      traverse a@(AnnVar v) | v `DS.member` vars = Nothing
                            | otherwise          = Just a
      traverse a@(AnnSet _) = Just a
      traverse b@(AnnUnion a a') = 
        case traverse a of
          Nothing -> case traverse a' of
                      Nothing -> Just AnnEmpty
                      Just x  -> Just x
          Just x  -> case traverse a' of
                      Nothing -> Just AnnEmpty
                      Just x' -> Just $ AnnUnion x x'
  in case traverse b of
      Nothing -> case traverse b' of
                  Nothing -> c
                  Just x  -> AnnUnion c x
      Just x  -> case traverse b' of
                  Nothing -> AnnUnion c x
                  Just x' -> AnnUnion c (AnnUnion x x')
                  
collectAnnVars :: SAnn -> Set AnnVar
collectAnnVars (AnnVar v)     = DS.singleton v
collectAnnVars (AnnUnion a b) = collectAnnVars a `DS.union` collectAnnVars b
collectAnnVars (AnnSet _ )    = DS.empty

containsAnnVar :: AnnVar -> SAnn -> Bool
containsAnnVar v (AnnVar v')    = v == v'
containsAnnVar v (AnnSet _)     = False 
containsAnnVar v (AnnUnion a b) = containsAnnVar v a || containsAnnVar v b

{-
solveConstraints :: Lattice a => Set ConstrPair -> Map SAnn a
solveConstraints c = absWl c eval 
  where
  eval annVar ana = DM.lookup annVar ana
-}

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

