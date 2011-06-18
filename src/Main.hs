module Main where

import CCO.Component    (printer, ioWrap, component)
import APA2.HM           (parser)
import APA2.AG
import Control.Arrow    (arr, (>>>))
import CCO.Tree                   (Tree (fromTree, toTree))
import qualified CCO.Tree as T    (ATerm (App))
import CCO.Tree.Parser            (parseTree, app, arg)
import Control.Applicative        (Applicative ((<*>)), (<$>))
import Data.Map (Map)
import qualified Data.Map as DM
import Debug.Trace

instance Tree Tm where
  fromTree (Var x)       = T.App "Var" [fromTree x]
  fromTree (Lam x t1)    = T.App "Lam" [fromTree x, fromTree t1]
  fromTree (App t1 t2)   = T.App "App" [fromTree t1, fromTree t2]
  fromTree (Let x t1 t2) = T.App "Let" [fromTree x, fromTree t1, fromTree t2]

  toTree = parseTree [ app "Var" (Var <$> arg                )
                     , app "Lam" (Lam <$> arg <*> arg        )
                     , app "App" (App <$> arg <*> arg        )
                     , app "Let" (Let <$> arg <*> arg <*> arg)
                     ]

convertAndType = component (return . doConversion)

doConversion :: Tm -> Ty
doConversion t = let inferredType = inferredType_Syn_Tm (wrap_Tm (sem_Tm t) inherit)
                     {- annotated    = annotated_Syn_Tm (wrap_Tm (sem_Tm t) inherit)-}
                     -- we must generalise one more time, to figure out which
                     -- TyLam's must be prepended. 
                     {- (ty', coercion) = generalise [] inferredType-}
                 {- in  trace ("Type of entire expression: \n  :: "++show ty'++"\n") -}
                     {- (coercion annotated)-}
                in inferredType
-- | The top-level inherited attribute to be passed to an attribute grammar
-- for System F. In our case, we want to start with an empty type
-- environment, and a variable counter of 0 (used for generating fresh
-- type variables).
inherit :: Inh_Tm
inherit = Inh_Tm { typeEnvironment_Inh_Tm = DM.empty -- Start with empty environment.
                 , counter_Inh_Tm = 0          -- Start variable-name-seed at 0.
                 }


printer' = component (return . show)

main = ioWrap (parser >>> convertAndType >>> printer')
