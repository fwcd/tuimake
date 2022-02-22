module Main where

import System.Environment (getArgs)
import Tuimake.App (runApp)

main :: IO ()
main = do
  args <- getArgs
  
  case args of
    ["--help"] -> putStrLn usage
    ["-h"]     -> putStrLn usage
    _          -> runApp
  
  where usage = "Usage: tuimake [target...]"
