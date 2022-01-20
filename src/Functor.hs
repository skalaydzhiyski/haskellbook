module Functor where

import Test.QuickCheck
import Test.QuickCheck.Function

replaceWithP = (\_ -> 'p')
dat          = [Just "something", Nothing, Just "something else"]
l1           = replaceWithP dat
l2           = fmap replaceWithP dat
l3           = (fmap.fmap) replaceWithP dat
l4           = (fmap.fmap.fmap) replaceWithP dat

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
     in (*3) <$> read <$> ("123" ++) <$> (show <$> ioi)

functorIdent :: (Functor f, Eq (f a)) => f a -> Bool
functorIdent f = fmap id f == id f

functorCompose :: (Eq (f c), Functor f) => (b -> c) -> (a -> b) -> f a -> Bool
functorCompose g f x = (fmap g (fmap f x)) == fmap (g.f) x

newtype Identity a = Identity a deriving (Eq, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

data Pair a = Pair a a deriving (Eq, Show)
instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)
instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    return $ Pair x x

data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
  fmap f (Two left right) = Two left (f right)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

data Three a b c = Three a b c deriving (Eq, Show)
instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

data Three' a b = Three' a b b deriving (Eq, Show)
instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
  fmap f (Four x y z q) = Four x y z (f q)

data Sum a b = First a | Second b deriving (Eq, Show)
instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second x) = Second (f x)
instance Arbitrary b => Arbitrary (Sum a b) where
  arbitrary = do
    x <- arbitrary
    return $ Second x

-- Wrapped functors
data Wrap f a = Wrap (f a) deriving (Eq, Show)
instance Functor f => Functor (Wrap f) where
  fmap func (Wrap f) = Wrap (fmap func f)


-- TODO: continue here with the IO functor part from the book
