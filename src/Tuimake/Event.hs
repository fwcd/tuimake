module Tuimake.Event
  ( MakeEvent (..)
  ) where

import qualified Data.Text as T
import Tuimake.State (TargetState)

-- | An asynchronously emitted event from the make process.
data MakeEvent = StdoutLine T.Text
               | StderrLine T.Text
               | EOF
               | TargetEntered TargetState T.Text
               | TargetUpdated TargetState T.Text
               | TargetExited T.Text
  deriving (Show, Eq)
