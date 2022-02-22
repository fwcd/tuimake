module Tuimake.Event
  ( MakeEvent (..)
  ) where

-- | An asynchronously emitted event from the make process.
data MakeEvent = StdoutLine String
               | StderrLine String
               | EOF
              -- TODO:
              --  | RuleEntered String
              --  | RuleExited String
