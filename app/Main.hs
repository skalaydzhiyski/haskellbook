{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Monoid
{-
---------------------- Semigroups ------------------------------------------
--
import Prelude hiding (Monoid, Semigroup(..), mappend, mempty)
import Data.Monoid hiding (Monoid(..), (<>), Sum(..), Product(..))

-- def: Semigroup is a type and a binary operation associated with it (<>)
class Semigroup a where
  (<>) :: a -> a -> a

-- example: for the case of the type List - the binary operator is concatenation (++)
instance Semigroup [a] where
  (<>) = (++)
instance Monoid [a] where
  mempty = []
  -- we don't need anything else, since mappend (<>) is already defined in the Semigroup instance for list!

-- example: for a new type we want to convert to semigroup - just implement (<>)
newtype Property a = Property a deriving (Show)
instance Num a => Semigroup (Property a) where
  (Property left) <> (Property right) = Property (left + right)

---------------------- Monoids ------------------------------------------

class Semigroup m => Monoid m where
  mempty  :: m
  mappend :: m -> m -> m
  mappend = (<>)

newtype Sum     a = Sum     {getSum :: a}     deriving (Show, Eq, Ord, Num)
newtype Product a = Product {getProduct :: a} deriving (Show, Eq, Ord, Num)

-- NOTE: Because we can have two DIFFERENT instances of Monoid for Numbers
--        (one for addition and one for multiplication) -> we have Numbers NOT as Monoid in Prelude
instance Num a => Semigroup (Sum a) where
  (<>) = (+)
instance Num a => Monoid (Sum a) where
  mempty = 0

instance Num a => Semigroup (Product a) where
  (<>) = (*)
instance Num a => Monoid (Product a) where
  mempty = 1
-}

-- examples for monoidal laws
assoc_sum  = Sum 1 <> (Sum 2 <> Sum 3)
              ==
             (Sum 1 <> Sum 2) <> Sum 3

assoc_prod = Product 1 <> (Product 2 <> Product 3)
              ==
             (Product 1 <> Product 2) <> Product 3

assoc_lst = ([1,2] <> [3,4]) <> [5,6]
                ==
               [1,2] <> ([3,4] <> [5,6])

assoc_string = ("ab" <> "cd") <> "ef"
                ==
               "ab" <> ("cd" <> "ef")

concat_sum = mconcat [Sum 2, Sum 3, Sum 4]
concat_prd = mconcat [Product 2, Product 3, Product 4]
concat_lst = mconcat [[1,2], [2,3], [4,5]]
concat_str = mconcat ["ab", "cd", "ef"]

main :: IO ()
main = undefined

