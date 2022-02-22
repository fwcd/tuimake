module Tuimake.Controller
  ( appEvent
  ) where

import qualified Brick.Main as BM
import qualified Brick.Types as BT
import qualified Graphics.Vty as V
import Tuimake.State (AppState (..))
import Tuimake.Process (MakeEvent (..))

-- | Handles an application event.
appEvent :: AppState -> BT.BrickEvent () MakeEvent -> BT.EventM () (BT.Next AppState)
appEvent st (BT.AppEvent e) = BM.continue $
  case e of
    StdoutLine l -> st { stOutput = l : stOutput st }
    StderrLine l -> st { stOutput = l : stOutput st }
    EOF          -> st { stExited = True }
appEvent st (BT.VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'c') [V.MCtrl] -> BM.halt st
    V.EvKey (V.KChar 'q') []        -> BM.halt st
    _                               -> BM.continue st
appEvent st _ = BM.continue st
