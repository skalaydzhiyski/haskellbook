module Main where

import Control.Monad (join)

-- this is basically (flip (>>=)) for when testing exmaples.
bind :: Monad m => (a -> m b) -> m a -> m b
bind f mx = undefined


main :: IO ()
main = do
  putStrLn "working brother"

