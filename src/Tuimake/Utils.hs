module Tuimake.Utils
  ( whileM_
  ) where

import Control.Monad (when)

-- | Loops while the given condition is true.
whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ cond body = do
    c <- cond
    when c $ do
        body
        whileM_ cond body
