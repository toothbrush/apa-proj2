module Main where

import Components

main :: IO ()
main = do
  input <- getContents
  (debugInference . parseProgram) input
  --print $ (inferTypes . parseProgram) input
