module Main where

import Control.Monad (join)

funcWorkingBrother :: [String] -> (Type, GHC.HaskellLanguage) -> Maybe Worker
funcWorkingBrother = undefined

-- this is basically (flip (>>=)) for when testing exmaples.
bind :: Monad m => (a -> m b) -> m a -> m b
bind f mx = undefined

-- TODO: Continue here when you come back to Haskell

main :: IO ()
main = do
  putStrLn "working brother"

