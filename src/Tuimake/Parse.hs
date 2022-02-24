{-# LANGUAGE OverloadedStrings #-}
module Tuimake.Parse
  ( parseEvent
  ) where

import qualified Data.Text as T
import Text.Parsec (Parsec, parse, string, spaces, noneOf, many1, (<|>), oneOf)
import Tuimake.Event (MakeEvent (..))
import Tuimake.State (TargetState (..))
import Tuimake.Utils (rightToMaybe)

type Parser = Parsec T.Text ()

-- | Parses an event from a line emitted by make.
parseEvent :: T.Text -> Maybe MakeEvent
parseEvent = rightToMaybe . parse event "<unnamed>"

event :: Parser MakeEvent
event = spaces *> (targetEntered <|> targetUpdated <|> targetExited)

targetEntered :: Parser MakeEvent
targetEntered = string "Considering target file" *> spaces *> (TargetEntered BuildingPrerequisites <$> targetName)

targetUpdated :: Parser MakeEvent
targetUpdated = string "Finished prerequisites of target file" *> spaces *> (TargetUpdated BuildingTarget <$> targetName)

targetExited :: Parser MakeEvent
targetExited = (string "Successfully remade target file" *> spaces *> (TargetExited <$> targetName))
           <|> (string "No need to remake target" *> spaces *> (TargetExited <$> targetName))

targetName :: Parser T.Text
targetName = T.pack <$> (oneOf "'`" *> many1 (noneOf "`'"))
