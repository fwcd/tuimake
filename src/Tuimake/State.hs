module Tuimake.State
  ( AppState (..)
  , initialState
  ) where

-- | The application's state.
data AppState = AppState
  { -- | The output lines of the make process.
    stOutput :: [String]
    -- | The current rule that make is processing.
  , stRuleStack :: [String]
  }

-- | The initial state when the app launches.
initialState :: AppState
initialState = AppState
  { stOutput = []
  , stRuleStack = []
  }

