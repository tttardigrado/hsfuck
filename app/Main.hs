module Main where

import Lang               ( BF )
import Parser             ( parseBF )
import Optimizer          ( optimizeBF )
import GenC               ( generateC )
import GenMIPS            ( generateMIPS )

import System.Environment ( getArgs )
import System.Exit        ( exitSuccess, exitFailure )

data Modes = C | MIPS
  deriving Show

genTarget :: Modes -> BF -> String
genTarget C    = generateC
genTarget MIPS = generateMIPS

compiler :: Modes -> String -> String -> IO ()
compiler mode file out = do
  src <- readFile file
  case parseBF src of
    Left err -> print err
    Right bf -> let optim = optimizeBF bf        in
                writeFile out $ genTarget mode optim

main :: IO ()
main = do
  args <- getArgs
  case args of
    "c"   :file:out:_ -> compiler C    file out
    "mips":file:out:_ -> compiler MIPS file out
    otherwise  -> fail "Please provide the target (c or mips), the program path and the output path as arguments"

