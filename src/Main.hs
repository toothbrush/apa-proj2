module Main where

import CCO.Component    (printer, ioWrap)
import CCO.HM           (parser)
import CCO.HM.AG
import Control.Arrow    (arr, (>>>))
import CCO.Tree                   (Tree (fromTree, toTree))
import qualified CCO.Tree as T    (ATerm (App))
import CCO.Tree.Parser            (parseTree, app, arg)
import Control.Applicative        (Applicative ((<*>)), (<$>))


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


main = ioWrap (parser >>> arr fromTree >>> printer)
