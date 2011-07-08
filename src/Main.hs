module Main where
import System.Environment ( getArgs )
import Language.Haskell.Exts.Parser
import Components

-- | Main
main :: IO ()
main = do
  args  <- getArgs
  input <- getContents
  case args of
    [] -> doDefault input
    xs -> case head xs of 
            "debug"    -> (debugInference . parseProgram) input
            "parse-mh" -> print $ parseProgram input
            "parse-hs" -> print $ parseExp input
            _          -> do putStrLn "Argument not recognized"
                             putStrLn usage

-- | Default action if no argument is provided to the executable. It parses the program, executes the analysis and prints the results.
doDefault :: String -> IO ()
doDefault input = (analysisResult . parseProgram)  input

-- | The help message
usage :: String
usage = "Usage: \n\n" ++
        "cfa cmd\n\n" ++
        "Where cmd is one of:\n\n" ++
        "debug       Shows debug output\n" ++
        "parse-mh    Parses the untyped lambda calculus and shows the output\n" ++
        "parse-hs    Parses Haskell source code and shows the output\n" ++
        "help        Shows this help text"