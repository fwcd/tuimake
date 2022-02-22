module Tuimake.Process
  ( runMake
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Brick.BChan as BC
import System.Process (createProcess, proc)
import System.IO (hGetLine)

-- | An asynchronously emitted event from the make process.
data MakeEvent = StdoutLine String
               | StderrLine String
              -- TODO:
              --  | RuleEntered String
              --  | RuleExited String

-- | Runs make asynchronously with the given args (targets) and
-- returns a channel through which events are emitted.
runMake :: [String] -> IO (BC.BChan MakeEvent)
runMake args = do
  chan <- BC.newBChan 10
  (_, Just out, Just err, _) <- createProcess $ proc "make" $ "--debug=v" : args

  -- TODO: Handle case where process terminates
  forkIO $ forever $ do
    line <- hGetLine out
    BC.writeBChan chan (StdoutLine line)

  -- TODO: Handle case where process terminates
  forkIO $ forever $ do
    line <- hGetLine err
    BC.writeBChan chan (StderrLine line)
  
  return chan
