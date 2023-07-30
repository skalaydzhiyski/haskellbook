module Main where

{-# language SomethingHere #-}

import Prelude hiding (lookup)
import Control.Monad (join, (>=>))
import Data.Map (fromList, lookup)


-- this is basically (flip (>>=)) for when testing exmaples.
bind :: Monad m => (a -> m b) -> m a -> m b
bind f mx = join $ fmap f mx

-- binding and binding' should have the output in the console
binding :: IO ()
binding = do
  value <- getLine
  value2 <- getLine
  putStrLn $ value ++ ['\n'] ++ value2

binding' :: IO ()
binding' = getLine >>= putStrLn >> getLine >>= putStrLn


bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "Enter something else here: "
  value <- getLine
  putStrLn $ "Your name is: " ++ value

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "Enter something else here: " >> getLine >>= (\value -> putStrLn $ "Your name is: " ++ value)


nameAge :: IO ()
nameAge = do
  putStrLn "enter your name: "
  name <- getLine
  putStrLn "enter your age: "
  age <- getLine
  putStrLn $ name ++ "\n" ++ age

-- do syntax is just to avoid the nesting below - behaviour is the same!
nameAge' :: IO ()
nameAge' =
  putStrLn "enter your name: " >>
  getLine >>=
    \name -> putStrLn "enter your age: " >>
      getLine >>=
        \age -> putStrLn $ name ++ "\n" ++ age


doubleEvens :: [Integer] -> [Integer]
doubleEvens xs = do
  x <- xs
  if even x then [x*x, x*x] else [x]

-- Maybe monad

data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
} deriving (Eq, Show)


noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

-- if Cow's name is Bess,
-- it must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499 then Nothing
  else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty -> weightCheck (Cow nammy agey weighty)

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  name   <- noEmpty name'
  age    <- noNegative age'
  weight <- noNegative weight'
  weightCheck (Cow name age weight)

-- Another test on the maybe Monad.
map_ = fromList $ zip [1..10] ["value" ++ (show x) | x <- [1..10]]

type InitialMessage = String
maybeSum :: InitialMessage -> Maybe String
maybeSum im = do
  val1 <- lookup 1 map_
  val2 <- lookup 2 map_
  val3 <- lookup 3 map_
  Just $ im ++ val1 ++ val2 ++ val3

-- Either monad
data Sum a b = First a | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second x) = Second (f x)
  fmap _ (First x) = First x

instance Applicative (Sum a) where
  pure = Second
  (Second f) <*> right = fmap f right
  (First x) <*> _ = First x

instance Monad (Sum a) where
  return = pure
  (Second x) >>= f = f x
  (First x) >>= _  = First x

-- The Beauty of Haskell --------------
list :: String
list = [z | x <- [1..3], y <- show x, z <- "number: " ++ [y] ++ ", "]
-- desugaring level 1
list' :: String
list' = do
  x <- [1..3]
  y <- show x
  z <- "number: " ++ [y] ++ ", "
  return z
-- desugaring level 2
list'' :: String
list'' =
  [1..3] >>=
    \x -> show x >>=
      \y -> "number: " ++ [y] ++ ", " >>=
        return
-- -------------------------------------

-- Implementation of Kliesli composition (>=>)
mcomp :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
mcomp f g x = g x >>= f

mcomp' :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
mcomp' = (>=>)

-- more examples
sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

-- exercises
data Nope a = NopeDotJpg

data BahEither b a = Pleft a | Pright b

data Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap = undefined

instance Applicative Identity where
  pure = undefined
  left <*> right = undefined

instance Monad Identity where
  return = pure
  idval >>= f = undefined

-- TODO: continue here when you're coming back to Applicatives and Monads
--       and try to make sure you understand Functor/Applicative/Monad
--       perfectly ! Once we've learned Haskell, we can delve into Plutus again.

f x y = 128 + x + y

main :: IO ()
main = do
  number <- getLine
  putStrLn "working brother"

