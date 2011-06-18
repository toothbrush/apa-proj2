module Main where
import Components

main = 
  do
    input <- getContents
    putStrLn $ ( (show . inferTypes . parseProgram) input )

