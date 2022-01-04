module Main where
import Data.List (intercalate)

notThe :: String -> Maybe String
notThe "the" = Just "a"
notThe x = Just x

replaceThe :: String -> String
replaceThe s = undefined

main :: IO ()
main = undefined

