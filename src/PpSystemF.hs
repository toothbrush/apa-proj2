import CCO.Component    (Component, component, printer, ioWrap)
import CCO.SystemF      (Tm)
import CCO.Tree         (ATerm, toTree, parser)
import Control.Arrow    (arr, (>>>))

main = ioWrap $
       parser >>> (component toTree :: Component ATerm Tm) >>> printer