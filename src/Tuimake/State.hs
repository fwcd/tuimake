module Tuimake.State
  ( AppState (..)
  , initialState
  ) where

-- | The application's state.
data AppState = AppState
  { -- | The output lines of the make process (in reverse chronological order).
    stOutput :: [String]
    -- | The current rule that make is processing (newest element is first).
  , stRuleStack :: [String]
    -- | Whether the app has exited.
  , stExited :: Bool
  }

-- | The initial state when the app launches.
initialState :: AppState
initialState = AppState
  { stOutput = []
  , stRuleStack = []
  , stExited = False
  }

