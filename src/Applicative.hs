module Applicative where

-- this is some comment brother
import Control.Applicative
import Data.List (elemIndex)

newtype Identity a = Identity a
  deriving (Show, Eq, Ord)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity $ f x

--

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Show, Ord)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant $ mempty
  left <*> right = Constant $ (getConstant left) <> (getConstant right)

--

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if (length s) > maxLen then Nothing else Just s

mkName :: String -> Maybe Name
mkName x = fmap Name $ validateLength 25 x

mkAddress :: String -> Maybe Address
mkAddress x = fmap Address $ validateLength 100 x

mkPerson :: String -> String -> Maybe Person
mkPerson name address = Person <$> mkName name <*> mkAddress address

