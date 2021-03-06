module Tuimake.Utils
  ( whileM_
  , rightToMaybe
  ) where

import Control.Monad (when)

-- | Loops while the given condition is true.
whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ cond body = do
    c <- cond
    when c $ do
        _ <- body
        whileM_ cond body

-- | Just if right, otherwise none.
rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right x) = Just x
