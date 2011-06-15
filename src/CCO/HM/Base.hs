-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.HM.Base
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A simple, implicitly typed functional language.
--
-------------------------------------------------------------------------------

module CCO.HM.Base (
    -- * Syntax
    Var                         -- = String
  , Tm (Tm)                     -- instances: Tree
  , Tm_ (Var, Lam, App, Let)    -- instances: Tree
) where

import CCO.Types
import CCO.HM.AG
import CCO.Tree                   (Tree (fromTree, toTree))
import qualified CCO.Tree as T    (ATerm (App))
import CCO.Tree.Parser            (parseTree, app, arg)
import Control.Applicative        (Applicative ((<*>)), (<$>))

-------------------------------------------------------------------------------
-- Tree instances
-------------------------------------------------------------------------------

instance Tree Tm where
  fromTree (Tm pos t) = T.App "Tm" [fromTree pos, fromTree t]
  toTree = parseTree [app "Tm" (Tm <$> arg <*> arg)]

instance Tree Tm_ where
  fromTree (Var x)       = T.App "Var" [fromTree x]
  fromTree (Lam x t1)    = T.App "Lam" [fromTree x, fromTree t1]
  fromTree (App t1 t2)   = T.App "App" [fromTree t1, fromTree t2]
  fromTree (Let x t1 t2) = T.App "Let" [fromTree x, fromTree t1, fromTree t2]

  toTree = parseTree [ app "Var" (Var <$> arg                )
                     , app "Lam" (Lam <$> arg <*> arg        )
                     , app "App" (App <$> arg <*> arg        )
                     , app "Let" (Let <$> arg <*> arg <*> arg)
                     ]
