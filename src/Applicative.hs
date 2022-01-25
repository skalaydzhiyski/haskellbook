module Applicative where

-- this is some comment brother
import Control.Applicative
import Data.List (elemIndex)

newtype Identity a = Identity a
  deriving (Show, Eq, Ord)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

-- TODO: read more exactly what the Identity type mean.
instance Applicative Identity where
  pure  = Identity
  (Identity f) <*> (Identity x) = Identity (f x)


-- TODO: Continue here with the chapter on Constant from the book.



