module Main where
    
    import Parser.Base
    import Parser.PrettyPrint
    import Text.ParserCombinators.UU
    import System
    
    main :: IO ()
    main = do
      putStrLn "Parser for FUN language.\n"
      (fileName:_) <- getArgs
      putStrLn $ show $fileToTerm fileName

    fileToTerm :: String -> Term
    fileToTerm fileName = Var "s"
