module Tuimake.Controller
  ( appEvent
  ) where

import qualified Brick.Main as BM
import qualified Brick.Types as BT
import qualified Graphics.Vty as V

-- | Handles an application event.
appEvent :: () -> BT.BrickEvent () e -> BT.EventM () (BT.Next ())
appEvent st (BT.VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'c') [V.MCtrl] -> BM.halt st
    V.EvKey (V.KChar 'q') []        -> BM.halt st
    _                               -> BM.continue st
appEvent st _ = BM.continue st
