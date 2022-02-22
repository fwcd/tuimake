module Tuimake.UI
  ( drawUI
  ) where

import qualified Brick.Types as BT
import qualified Brick.Widgets.Border as BW
import qualified Brick.Widgets.Border.Style as BW
import qualified Brick.Widgets.Center as BW
import qualified Brick.Widgets.Core as BW
import Brick.Widgets.Core ((<+>))

drawUI :: () -> BT.Widget ()
drawUI st =
  BW.withBorderStyle BW.unicode $
    BW.hBox
      [ BW.hLimitPercent 30 $ BW.borderWithLabel (BW.str "Rule Stack") $ BW.padTop BT.Max $
          BW.vBox $
            BW.padRight BT.Max <$>
              [ BW.str "abc"
              , BW.str "another demo"
              ]
      , BW.center (BW.str "Test")
      ]
