module Main where
import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

checkUTC (DbDate _ ) = True 
checkUTC _           = False

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map (\(DbDate val) -> val) . filter checkUTC


main :: IO ()
main = do
  putStrLn "work"
