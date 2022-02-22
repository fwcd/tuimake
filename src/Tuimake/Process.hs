module Tuimake.Process
  ( runMake
  ) where

import qualified Brick.BChan as BC
import Control.Concurrent (forkIO)
import Control.Exception (catchJust)
import Control.Monad (forever)
import Data.Maybe (mapMaybe)
import System.Process (createProcess, proc, CreateProcess (..), StdStream (..))
import System.IO (hGetLine, hReady, hIsEOF, Handle)
import System.IO.Error (isEOFError)
import Tuimake.Event (MakeEvent (..))
import Tuimake.Parse (parseEvent)
import Tuimake.Utils (whileM_)

-- | Catches an EOF.
catchEOF :: IO a -> IO a -> IO a
catchEOF onEOF x = catchJust (\e -> if isEOFError e then Just () else Nothing) x (const onEOF)

-- | Reads all available lines from the handle. If there are none, blocks.
hGetLines :: Handle -> IO [String]
hGetLines h = catchEOF (return []) $ do
  l <- hGetLine h
  r <- hReady h
  if r then (l :) <$> hGetLines h else return [l]

-- | Runs make asynchronously with the given args (targets) and
-- returns a channel through which events are emitted.
runMake :: [String] -> IO (BC.BChan [MakeEvent])
runMake args = do
  -- Create a channel and spawn the process
  chan <- BC.newBChan 10
  (_, Just out, Just err, _) <- createProcess (proc "make" ("--debug=v" : args))
    { std_out = CreatePipe
    , std_err = CreatePipe
    , std_in  = NoStream
    }

  -- Read stdout and pass it to the channel on a separate thread
  _ <- forkIO $ do
    whileM_ (not <$> hIsEOF out) $ do
      ls <- hGetLines out
      let events = mapMaybe parseEvent ls ++ (StdoutLine <$> ls)
      BC.writeBChan chan events
    BC.writeBChan chan [EOF]

  -- Read stderr and pass it to the channel on a separate thread
  _ <- forkIO $ whileM_ (not <$> hIsEOF err) $ do
    ls <- hGetLines err
    let events = StderrLine <$> ls
    BC.writeBChan chan events
  
  return chan
