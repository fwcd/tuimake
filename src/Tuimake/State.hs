{-# LANGUAGE OverloadedLists #-}
module Tuimake.State
  ( AppState (..)
  , TargetState (..)
  , initialState
  ) where

import qualified Deque.Strict as D

-- | The state of a target.
data TargetState = BuildingPrerequisites
                 | BuildingTarget
  deriving (Show, Eq)

-- | The application's state.
data AppState = AppState
  { -- | The output lines of the make process (in chronological order).
    stOutput :: D.Deque String
    -- | The current target that make is processing (newest element is first).
  , stTargetStack :: [(TargetState, String)]
    -- | Whether the app has exited.
  , stExited :: Bool
    -- | The percentage size of the target stack (the 'side bar')
  , stSplitPercentage :: Int
  }

-- | The initial state when the app launches.
initialState :: AppState
initialState = AppState
  { stOutput = []
  , stTargetStack = []
  , stExited = False
  , stSplitPercentage = 30
  }
