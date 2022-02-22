module Tuimake.App
  ( runApp
  ) where

import Control.Monad (void)
import qualified Brick.AttrMap as BA
import qualified Brick.Main as BM
import qualified Graphics.Vty as V
import Tuimake.Controller (handleEvent)
import Tuimake.Event (MakeEvent)
import Tuimake.UI (ViewId, drawUI)
import Tuimake.Process (runMake)
import Tuimake.State (AppState (..), initialState)

-- | The application structure.
theApp :: BM.App AppState MakeEvent ViewId
theApp = BM.App
  { BM.appDraw = return . drawUI
  , BM.appChooseCursor = BM.neverShowCursor
  , BM.appHandleEvent = handleEvent
  , BM.appStartEvent = return
  , BM.appAttrMap = const $ BA.attrMap V.defAttr []
  }

-- | Runs the application, taking over the terminal screen.
runApp :: [String] -> IO ()
runApp args = do
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  chan <- runMake args
  void $ BM.customMain initialVty buildVty (Just chan) theApp initialState
