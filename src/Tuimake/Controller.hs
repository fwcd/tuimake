module Tuimake.Controller
  ( appEvent
  ) where

import qualified Brick.Main as BM
import qualified Brick.Types as BT
import qualified Graphics.Vty as V
import Tuimake.UI (ViewId, scrollOutput)
import Tuimake.State (AppState (..))
import Tuimake.Process (MakeEvent (..))

-- | Handles an application event.
appEvent :: AppState -> BT.BrickEvent ViewId MakeEvent -> BT.EventM ViewId (BT.Next AppState)
appEvent st (BT.AppEvent e) = BM.continue $
  case e of
    StdoutLine l -> st { stOutput = l : stOutput st }
    StderrLine l -> st { stOutput = l : stOutput st }
    EOF          -> st { stExited = True }
appEvent st (BT.VtyEvent e) =
  let scrollUp   = BM.vScrollBy scrollOutput (-1) >> BM.continue st
      scrollDown = BM.vScrollBy scrollOutput 1    >> BM.continue st
  in case e of
    V.EvKey (V.KChar 'k') []        -> scrollUp
    V.EvKey (V.KUp)       []        -> scrollUp
    V.EvKey (V.KChar 'j') []        -> scrollDown
    V.EvKey (V.KDown)     []        -> scrollDown
    V.EvKey (V.KChar 'c') [V.MCtrl] -> BM.halt st
    V.EvKey (V.KChar 'q') []        -> BM.halt st
    _                               -> BM.continue st
appEvent st _ = BM.continue st
