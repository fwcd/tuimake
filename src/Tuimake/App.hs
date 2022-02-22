module Tuimake.App
  ( runApp
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import qualified Brick.AttrMap as BA
import qualified Brick.Main as BM
import qualified Brick.Types as BT
import qualified Brick.Widgets.Core as BW

initialState :: ()
initialState = ()

appEvent :: () -> BT.BrickEvent () e -> BT.EventM () (BT.Next ())
appEvent st (BT.VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'c') [V.MCtrl] -> BM.halt st
    V.EvKey (V.KChar 'q') []        -> BM.halt st
    _                               -> BM.continue st
appEvent st _ = BM.continue st

drawUI :: () -> [BT.Widget ()]
drawUI st = [BW.str "Test"]

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
