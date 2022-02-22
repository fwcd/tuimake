module Main where

import System.Environment
import Lib

main :: IO ()
main = do
  args <- getArgs
  
  case args of
    ["--help"] -> putStrLn usage
    ["-h"]     -> putStrLn usage
    _          -> putStrLn "TODO"
  
  where usage = "Usage: tuimake [target...]"
