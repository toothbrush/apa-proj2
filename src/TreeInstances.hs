module TreeInstances where

import Base
import Control.Applicative
import CCO.Tree                    (Tree (fromTree, toTree))
import qualified CCO.Tree as T     (ATerm (App))
import CCO.Tree.Parser             (parseTree, app, arg)
import qualified Infer.Typed as SF (Tm(..),Ty(..))

instance Tree Term where
  fromTree (Var x)       = T.App "Var" [fromTree x]
  fromTree (Lam x t1)    = T.App "Lam" [fromTree x, fromTree t1]
  fromTree (App t1 t2)   = T.App "App" [fromTree t1, fromTree t2]
  fromTree (Let x t1 t2) = T.App "Let" [fromTree x, fromTree t1, fromTree t2]
  toTree = parseTree [ app "Var" (Var <$> arg                )
                     , app "Lam" (Lam <$> arg <*> arg        )
                     , app "App" (App <$> arg <*> arg        )
                     , app "Let" (Let <$> arg <*> arg <*> arg)
                     ]

instance Tree SF.Ty where
  fromTree (SF.TyVar a)      = T.App "TyVar" [fromTree a]
  fromTree (SF.Arr ty1 ty2)  = T.App "Arr" [fromTree ty1, fromTree ty2]
  fromTree (SF.Forall a ty1) = T.App "Forall" [fromTree a, fromTree ty1]
  toTree = parseTree [ app "TyVar"  (SF.TyVar  <$> arg        )
                     , app "Arr"    (SF.Arr    <$> arg <*> arg)
                     , app "Forall" (SF.Forall <$> arg <*> arg)
                     ]

instance Tree SF.Tm where
  fromTree (SF.Var x)       = T.App "Var"   [fromTree x]
  fromTree (SF.Lam x ty t1) = T.App "Lam"   [fromTree x, fromTree ty, fromTree t1]
  fromTree (SF.App t1 t2)   = T.App "App"   [fromTree t1, fromTree t2]
  fromTree (SF.TyLam a t1)  = T.App "TyLam" [fromTree a, fromTree t1]
  fromTree (SF.TyApp t1 ty) = T.App "TyApp" [fromTree t1, fromTree ty]
  toTree = parseTree [ app "Var"   (SF.Var <$> arg                )
                     , app "Lam"   (SF.Lam <$> arg <*> arg <*> arg)
                     , app "App"   (SF.App <$> arg <*> arg        )
                     , app "TyLam" (SF.TyLam <$> arg <*> arg      )
                     , app "TyApp" (SF.TyApp <$> arg <*> arg      )
                     ]

