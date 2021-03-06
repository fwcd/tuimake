{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Tuimake.UI
  ( ViewId
  , scrollOutput
  , drawUI
  ) where

import qualified Brick.Main as BM
import qualified Brick.Types as BT
import qualified Brick.Widgets.Border as BW
import qualified Brick.Widgets.Border.Style as BW
import qualified Brick.Widgets.Core as BW
import qualified Data.Text as T
import GHC.Exts (toList)
import Tuimake.State (AppState (..), TargetState (..))

-- | Identifies a viewport.
data ViewId = VPOutput
  deriving (Ord, Show, Eq)

scrollOutput :: BM.ViewportScroll ViewId
scrollOutput = BM.viewportScroll VPOutput

-- | The widget for a target in the stack.
targetWidget :: TargetState -> T.Text -> BT.Widget ViewId
targetWidget BuildingPrerequisites = BW.txt -- TODO: Colors
targetWidget BuildingTarget = BW.txt . (<> " (building...)")

-- | Builds the UI tree from the state.
drawUI :: AppState -> BT.Widget ViewId
drawUI AppState {..} =
  BW.withBorderStyle BW.unicode $
    BW.hBox
      [ BW.hLimitPercent stSplitPercentage $
          BW.borderWithLabel (BW.str "Target Stack") $
            BW.padTop BT.Max $
              BW.vBox $
                BW.padRight BT.Max <$>
                  if null stTargetStack
                    then [BW.str "(none)"]
                    else uncurry targetWidget <$> stTargetStack
      , BW.padAll 1 $
          BW.viewport VPOutput BT.Vertical $
            BW.vBox $
              BW.padRight BT.Max . BW.txtWrap <$> toList stOutput
      ]
