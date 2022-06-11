{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Monoid_Semigroup where

import Data.Coerce
import Data.Monoid
import Test.Hspec
import Test.QuickCheck

{-
---------------------- Semigroups ------------------------------------------
--
import Prelude hiding (Monoid, Semigroup(..), mappend, mempty)
import Data.Monoid hiding (Monoid(..), (<>), Sum(..), Product(..))

-- def: Semigroup is a type and a binary operation associated with it (<>)
class Semigroup a where
  (<>) :: a -> a -> a

-- example: for the case of the type List - the binary operator is concatenation ++ for lists
instance Semigroup [a] where
  (<>) = (++) -- this should always be associative. Example: ([1] ++ [2]) ++ [3] == [1] ++ ([2] ++ [3])

instance Monoid [a] where
  mempty = [] -- this should be always be commutative Example: [] ++ [1,2,3] == [1,2,3] ++ []
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

-- Booleans
newtype All = All { getAll :: Bool } deriving (Show, Eq, Ord, Bounded)
newtype Any = Any { getAny :: Bool } deriving (Show, Eq, Ord, Bounded)

instance Semigroup All where
  (<>) = coerce (&&)
instance Monoid All where
  mempty = All True

instance Semigroup Any where
  (<>) = coerce (||)
instance Monoid Any where
  mempty = Any False
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

concat_bool = mconcat [All True, All True, All False]
concat_bool2 = mconcat [Any True, Any True, Any False]

------------------------------------ The Maybe Monoid -----------------------------------------------------

append_maybe = First (Just 1) <> First (Just 2)
append_maybe2 = Last (Just 1) <> Last (Just 2)
concat_maybe = mconcat [First (Just 1) , First (Just 2), First (Just 3)]
concat_maybe2 = mconcat [Last (Just 1) , Last (Just 2), Last (Nothing)]

-- + Advanced example of another Maybe monoind
--
-- combining values of Maybe ..
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (Only left) <> (Only right) = Only (left <> right)
  left <> Nada = left
  Nada <> right = right

instance Monoid a => Monoid (Optional a) where
  mempty  = Nada

-- mad libs exercise

type Exclamation = String
type Adverb = String
type Noun = String
type Adjective = String

-- ignore exercise too much typeing
madlibbin :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin e adv noun adj = undefined -- solution is to combine all strings and arguments in list and do mconcat :/

-- testing associativity with Quickcheck

asc :: (Eq a) => (a -> a -> a) -> a -> a -> a -> Bool
asc f a b c = f (f a b) c == f a (f b c)

monoidAsc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAsc a b c = (a <> (b <>c)) == ((a <> b) <> c)

checkIdent :: (Eq m, Monoid m) => m -> Bool
checkIdent x = left == right && left == x
  where left = mempty <> x
        right = x <> mempty

qc :: IO ()
qc = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1+1) > 1 `shouldBe` True
  describe "Subtraction" $ do
    it "1 + 1 is 0" $ do
      (1-1) == 0 `shouldBe` True
    it "all x+1 bigger than x" $ do
      property $ \x -> x + 1 > (x :: Int)


-- last exercises of chapter 15

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
qc_triv = do
  quickCheck (semigroupAssoc :: TrivAssoc)


newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem f' = Mem $ func
    where func = \x -> ((fst . f) x <> (fst . f') x,
                         snd . f' . snd . f $ x)

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\x -> (mempty, x))

f' = Mem $ \x -> ("hi", x + 1)

run_test :: IO ()
run_test = do
  let rmzero  = runMem mempty         0
      rmleft  = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft                    -- ("hi", 1)
  print $ rmright                   -- ("hi", 1)
  print $ (rmzero :: (String, Int)) -- ("", 0)
  print $ rmleft  == runMem f' 0    -- True
  print $ rmright == runMem f' 0    -- True

