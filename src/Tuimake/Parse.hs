module Tuimake.Parse
  ( parseEvent
  ) where

import Text.Parsec (Parsec, parse, string, spaces, noneOf, many1, char, (<|>))
import Tuimake.Event (MakeEvent (..))
import Tuimake.Utils (rightToMaybe)

type Parser = Parsec String ()

-- | Parses an event from a line emitted by make.
parseEvent :: String -> Maybe MakeEvent
parseEvent = rightToMaybe . parse event "<unnamed>"

event :: Parser MakeEvent
event = spaces *> (ruleEntered <|> ruleExited)

ruleEntered :: Parser MakeEvent
ruleEntered = string "Must remake target" *> spaces *> (RuleEntered <$> targetName)

ruleExited :: Parser MakeEvent
ruleExited = string "Successfully remade target file" *> spaces *> (RuleExited <$> targetName)

targetName :: Parser String
targetName = char '`' *> many1 (noneOf "'")
