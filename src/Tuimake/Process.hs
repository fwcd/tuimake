module Tuimake.Process
  ( runMake
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (catchJust)
import Control.Monad (forever)
import qualified Brick.BChan as BC
import qualified Data.Text as T
import System.Process (createProcess, proc, CreateProcess (..), StdStream (..))
import System.IO (hGetLine)
import System.IO.Error (isEOFError)
import Tuimake.Event (MakeEvent (..))
import Tuimake.Parse (parseEvent)

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
    }

  -- Read stdout and pass it to the channel on a separate thread
  _ <- forkIO $ catchEOF (BC.writeBChan chan EOF) $ forever $ do
    line <- T.pack <$> hGetLine out

    case parseEvent line of
      Just e  -> BC.writeBChan chan e
      Nothing -> return ()
    
    BC.writeBChan chan $ StdoutLine line

  -- Read stderr and pass it to the channel on a separate thread
  _ <- forkIO $ catchEOF (return ()) $ forever $ do
    line <- T.pack <$> hGetLine err
    BC.writeBChan chan (StderrLine line)
  
  return chan
