module Tuimake.Process
  ( MakeEvent (..)
  , runMake
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (catchJust)
import Control.Monad (forever)
import qualified Brick.BChan as BC
import System.Process (createProcess, proc, CreateProcess (..), StdStream (..))
import System.IO (hWaitForInput, hGetLine)
import System.IO.Error (isEOFError)

-- | An asynchronously emitted event from the make process.
data MakeEvent = StdoutLine String
               | StderrLine String
               | EOF
              -- TODO:
              --  | RuleEntered String
              --  | RuleExited String

-- | Catches an EOF.
catchEOF :: IO () -> IO () -> IO ()
catchEOF onEOF x = catchJust (\e -> if isEOFError e then Just () else Nothing) x (const onEOF)

-- | Runs make asynchronously with the given args (targets) and
-- returns a channel through which events are emitted.
runMake :: [String] -> IO (BC.BChan MakeEvent)
runMake args = do
  -- Create a channel and spawn the process
  chan <- BC.newBChan 10
  (_, Just out, Just err, _) <- createProcess (proc "make" ("--debug=v" : args))
    { std_out = CreatePipe
    , std_err = CreatePipe
    , std_in  = NoStream
    }

  -- Read stdout and pass it to the channel on a separate thread
  forkIO $ catchEOF (BC.writeBChan chan EOF) $ forever $ do
    line <- hGetLine out
    BC.writeBChan chan (StdoutLine line)

  -- Read stderr and pass it to the channel on a separate thread
  forkIO $ catchEOF (return ()) $ forever $ do
    line <- hGetLine err
    BC.writeBChan chan (StderrLine line)
  
  return chan
