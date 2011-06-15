{-# LANGUAGE Rank2Types,
             FlexibleContexts #-}

module Main where

import Base
import TreeInstances
import Parser.ParseRules
import Parser.PrettyPrint
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances
import CCO.Tree
import System
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "Parser for FUN language.\n"
  (fileName:_) <- getArgs
  fileContents <- readFile fileName
  let term = runParser fileName pTerm fileContents
  putStrLn (show$fromTree term)

fileToTerm :: String -> Term
fileToTerm fileName = Var "s"

run :: Show t =>  Parser t -> String -> IO ()
run p inp = do  let r@(a, errors) =  parse ( (,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) inp)
                putStrLn ("--  Result: " ++ show a)
                if null errors then  return ()
                               else  do putStr ("--  Correcting steps: \n")
                                        show_errors errors
                putStrLn "-- "
             where show_errors :: (Show a) => [a] -> IO ()
                   show_errors = sequence_ . (map (putStrLn . show))
