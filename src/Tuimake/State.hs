module Tuimake.State
  ( AppState (..)
  , TargetState (..)
  , initialState
  ) where

-- | The state of a target.
data TargetState = BuildingPrerequisites
                 | BuildingTarget
  deriving (Show, Eq)

-- | The application's state.
data AppState = AppState
  { -- | The output lines of the make process (in reverse chronological order).
    stOutput :: [String]
    -- | The current target that make is processing (newest element is first).
  , stTargetStack :: [(TargetState, String)]
    -- | Whether the app has exited.
  , stExited :: Bool
  }

-- | The initial state when the app launches.
initialState :: AppState
initialState = AppState
  { stOutput = []
  , stTargetStack = []
  , stExited = False
  }

