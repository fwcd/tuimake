{-# LANGUAGE OverloadedStrings #-}
module Tuimake.App
  ( runApp
  ) where

import qualified Brick.AttrMap as BA
import qualified Brick.Main as BM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Deque.Strict as D
import qualified Graphics.Vty as V
import GHC.Exts (toList)
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
  -- Spawn make process in the background
  chan <- runMake args

  -- Run the TUI
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  st <- BM.customMain initialVty buildVty (Just chan) theApp initialState

  -- Output last lines once quitting
  let output     = stOutput st
      limit      = 10
      isComplete = length output <= limit
      output'    = (if isComplete then id
                                  else D.cons $ "... (" <> T.pack (show (length output - limit)) <> " more lines) ...")
                 . D.reverse
                 . D.take limit
                 $ D.reverse output
  mapM_ TIO.putStrLn $ toList output'
