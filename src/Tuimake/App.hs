module Tuimake.App
  ( runApp
  ) where

import Control.Monad (void)
import qualified Graphics.Vty as V
import qualified Brick.AttrMap as BA
import qualified Brick.Main as BM
import qualified Brick.Types as BT

initialState :: ()
initialState = ()

appEvent :: () -> BT.BrickEvent () e -> BT.EventM () (BT.Next ())
appEvent st _ = BM.continue st

drawUI :: () -> [BT.Widget ()]
drawUI st = []

theApp :: BM.App () e ()
theApp = BM.App
  { BM.appDraw = drawUI
  , BM.appChooseCursor = BM.neverShowCursor
  , BM.appHandleEvent = appEvent
  , BM.appStartEvent = return
  , BM.appAttrMap = const $ BA.attrMap V.defAttr []
  }

runApp :: IO ()
runApp = void $ BM.defaultMain theApp initialState
