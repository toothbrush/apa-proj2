module Main where
import System.Environment ( getArgs )
import Language.Haskell.Exts.Parser
import Components

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
            _          -> do putStrLn ("Argument not recognized")
                             putStrLn usage

doDefault :: String -> IO ()
doDefault input = (analysisResult . parseProgram)  input

usage :: String
usage = "no usage description yet"
