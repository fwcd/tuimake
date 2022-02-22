module Tuimake.Controller
  ( appEvent
  ) where

import qualified Brick.Main as BM
import qualified Brick.Types as BT
import qualified Graphics.Vty as V
import Tuimake.Event (MakeEvent (..))
import Tuimake.UI (ViewId, scrollOutput)
import Tuimake.State (AppState (..))

-- | Handles an application event.
appEvent :: AppState -> BT.BrickEvent ViewId MakeEvent -> BT.EventM ViewId (BT.Next AppState)
appEvent st (BT.AppEvent e) =
  let appendLine l = BM.vScrollToEnd scrollOutput >> BM.continue st { stOutput = l : stOutput st }
  in case e of
    StdoutLine l  -> appendLine l
    StderrLine l  -> appendLine l
    RuleEntered t -> BM.continue $ st { stRuleStack = t : stRuleStack st }
    -- TODO: Check if RuleExited target matches top of stack
    RuleExited _  -> BM.continue $ st { stRuleStack = tail $ stRuleStack st }
    EOF           -> BM.continue $ st { stExited = True }
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
