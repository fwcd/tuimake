module Tuimake.UI
  ( drawUI
  ) where

import qualified Brick.Types as BT
import qualified Brick.Widgets.Border as BW
import qualified Brick.Widgets.Border.Style as BW
import qualified Brick.Widgets.Center as BW
import qualified Brick.Widgets.Core as BW

drawUI :: () -> BT.Widget ()
drawUI st =
  BW.withBorderStyle BW.unicode $
    BW.borderWithLabel (BW.str "Hello") (BW.center (BW.str "Hello"))
