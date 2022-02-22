{-# LANGUAGE RecordWildCards #-}
module Tuimake.UI
  ( ViewId
  , scrollOutput
  , drawUI
  ) where

import qualified Brick.Main as BM
import qualified Brick.Types as BT
import qualified Brick.Widgets.Border as BW
import qualified Brick.Widgets.Border.Style as BW
import qualified Brick.Widgets.Center as BW
import qualified Brick.Widgets.Core as BW
import Brick.Widgets.Core ((<+>))
import Tuimake.State (AppState (..))

-- | Identifies a viewport.
data ViewId = VPOutput
  deriving (Ord, Show, Eq)

scrollOutput :: BM.ViewportScroll ViewId
scrollOutput = BM.viewportScroll VPOutput

-- | Builds the UI tree from the state.
drawUI :: AppState -> BT.Widget ViewId
drawUI AppState {..} =
  BW.withBorderStyle BW.unicode $
    BW.hBox
      [ BW.hLimitPercent 30 $ BW.borderWithLabel (BW.str "Rule Stack") $ BW.padTop BT.Max $
          BW.vBox $
            BW.padRight BT.Max <$>
              if null stRuleStack
                then [BW.str "-- none --"]
                else BW.str <$> stRuleStack
      , BW.padAll 1 $
          BW.viewport VPOutput BT.Vertical $
            BW.strWrap $ unlines $ reverse stOutput
      ]
