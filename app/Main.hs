module Main where

import Control.Monad (join)

func :: String -> String -> [(a,b)]
func x y = undefined

-- this is basically (flip (>>=)) for testing exmaples.
bind :: Monad m => (a -> m b) -> m a -> m b
bind f mx = (+1) <$> Just 1280 <*> [1..1280]

main :: IO ()
main = do
  putStrLn "working brother"
