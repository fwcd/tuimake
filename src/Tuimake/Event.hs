module Tuimake.Event
  ( MakeEvent (..)
  ) where

-- | An asynchronously emitted event from the make process.
data MakeEvent = StdoutLine String
               | StderrLine String
               | EOF
               | RuleEntered String
               | RuleExited String
  deriving (Show, Eq)
