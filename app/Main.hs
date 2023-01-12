module Main where

import System.Environment ( getArgs )
import Parser ( parseBF )
import Optimizer ( optimizeBF )
import Generator ( generateC )


compiler :: String -> String -> IO ()
compiler file out = do
  src <- readFile file
  case parseBF src of
    Left err -> print err
    Right bf -> let c = generateC $ optimizeBF bf in
                writeFile out c


main :: IO ()
main = do
  args <- getArgs
  case args of
    file:out:_ -> compiler file out
    otherwise  -> fail "Please provide the program path and the output path as arguments"

