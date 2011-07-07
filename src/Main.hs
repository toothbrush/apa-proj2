module Main where

import Components

main :: IO ()
main = do
  input <- getContents
  let p = parseProgram input
  print p
  putStrLn "\n"
  analysisResult p
