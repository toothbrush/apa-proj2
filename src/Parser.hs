{-# LANGUAGE Rank2Types,
             FlexibleContexts #-}

module Main where

import Parser.Base
import Parser.ParseRules
import Parser.PrettyPrint
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances
import System

main :: IO ()
main = do
  putStrLn "Parser for FUN language.\n"
  (fileName:_) <- getArgs
  fileContents <- readFile fileName
  run pTerm fileContents

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
