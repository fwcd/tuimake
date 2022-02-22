module Tuimake.Parse
  ( parseEvent
  ) where

import Text.Parsec (Parsec, parse, string, spaces, noneOf, many1, char, (<|>))
import Tuimake.Event (MakeEvent (..))
import Tuimake.State (TargetState (..))
import Tuimake.Utils (rightToMaybe)

type Parser = Parsec String ()

-- | Parses an event from a line emitted by make.
parseEvent :: String -> Maybe MakeEvent
parseEvent = rightToMaybe . parse event "<unnamed>"

event :: Parser MakeEvent
event = spaces *> (targetEntered <|> targetUpdated <|> targetExited)

targetEntered :: Parser MakeEvent
targetEntered = string "Considering target file" *> spaces *> (TargetEntered BuildingPrerequisites <$> targetName)

targetUpdated :: Parser MakeEvent
targetUpdated = string "Finished prerequisites of target file" *> spaces *> (TargetUpdated BuildingTarget <$> targetName)

targetExited :: Parser MakeEvent
targetExited = string "Successfully remade target file" *> spaces *> (TargetExited <$> targetName)

targetName :: Parser String
targetName = char '`' *> many1 (noneOf "'")
