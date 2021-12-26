{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.Time
import Data.List

data Price = Int
  deriving (Eq, Show)

data Size = Size (Int, Int)
  deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Show, Eq)

data Airline =
    PapuAir
  | Catapults
  | TakeYourChances
  deriving (Show, Eq)

data Vehicle =
    Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar  _        = False

cars :: [Vehicle] -> [Bool]
cars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _         = error "no manufacturer present in the type class"

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance (Num a, Ord a, TooMany a) => TooMany (a, b) where
  tooMany x = (fst x) > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
newtype Cows = Cows Int deriving (Eq, Show)

func :: Int -> Bool
func n = n > 42

main :: IO ()
main = do
  putStrLn $ "res: "

