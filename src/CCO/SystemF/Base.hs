-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.SystemF.Base
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- System F.
--
-------------------------------------------------------------------------------

module CCO.SystemF.Base (
    -- * Syntax
    TyVar                               -- = String
  , Var                                 -- = String
  , Ty (TyVar, Arr, Forall)             -- instances: Tree
  , Tm (Var, Lam, App, TyLam, TyApp)    -- instances: Tree, Printable
) where

import CCO.SystemF.AG
import CCO.Printing               (Printable (pp))
import CCO.Tree                   (Tree (fromTree, toTree))
import qualified CCO.Tree as T    (ATerm (App))
import CCO.Tree.Parser            (parseTree, app, arg)
import Control.Applicative        (Applicative ((<*>)), (<$>))

-------------------------------------------------------------------------------
-- Tree instances
-------------------------------------------------------------------------------

instance Tree Ty where
  fromTree (TyVar a)      = T.App "TyVar" [fromTree a]
  fromTree (Arr ty1 ty2)  = T.App "Arr" [fromTree ty1, fromTree ty2]
  fromTree (Forall a ty1) = T.App "Forall" [fromTree a, fromTree ty1]

  toTree = parseTree [ app "TyVar"  (TyVar  <$> arg        )
                     , app "Arr"    (Arr    <$> arg <*> arg)
                     , app "Forall" (Forall <$> arg <*> arg)
                     ]

instance Tree Tm where
  fromTree (Var x)       = T.App "Var"   [fromTree x]
  fromTree (Lam x ty t1) = T.App "Lam"   [fromTree x, fromTree ty, fromTree t1]
  fromTree (App t1 t2)   = T.App "App"   [fromTree t1, fromTree t2]
  fromTree (TyLam a t1)  = T.App "TyLam" [fromTree a, fromTree t1]
  fromTree (TyApp t1 ty) = T.App "TyApp" [fromTree t1, fromTree ty]

  toTree = parseTree [ app "Var"   (Var <$> arg                )
                     , app "Lam"   (Lam <$> arg <*> arg <*> arg)
                     , app "App"   (App <$> arg <*> arg        )
                     , app "TyLam" (TyLam <$> arg <*> arg      )
                     , app "TyApp" (TyApp <$> arg <*> arg      )
                     ]

-------------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------------

instance Printable Tm where
  pp t = ppML_Syn_Tm (wrap_Tm (sem_Tm t) inhTm)

-------------------------------------------------------------------------------
-- Top-level inherited attributes
-------------------------------------------------------------------------------

-- | The top-level inherited attributes to be passed to an attribute grammar
-- for System F.
inhTm :: Inh_Tm
inhTm = Inh_Tm {prec_Inh_Tm = 0}
