module Tuimake.Event
  ( MakeEvent (..)
  ) where

import Tuimake.State (TargetState)

-- | An asynchronously emitted event from the make process.
data MakeEvent = StdoutLine String
               | StderrLine String
               | EOF
               | TargetEntered TargetState String
               | TargetUpdated TargetState String
               | TargetExited String
  deriving (Show, Eq)
