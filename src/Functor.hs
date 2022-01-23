{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Functor where

import Test.QuickCheck
import Test.QuickCheck.Function
import GHC.Arr

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

-- IO Functor

func1 :: IO String
func1 = do
  line <- getLine
  return (line ++ " and me too!")

-- Let's try and keep the values, but change the structure (opposite of fmap) well ...
-- this does not work :/
--    nat :: (f -> g) -> f a -> g a
--    nat = undefined
-- alternative:

-- this is coming from RankNTypes language extension
funcAllowed :: forall a . Maybe a -> [a]
funcAllowed Nothing  = []
funcAllowed (Just x) = [x]


-- Chapter exercises
--
newtype Mu f = InF { outF :: f (Mu f) } -- nope (kind (* -> *) -> *)
data D = D (Array Word Word) Int Int    -- nope (kind *)

-- rearrange type constrctor args
data S b a =
    Fst a
  | Snd b
  deriving (Show, Eq)

instance Functor (S a) where
  fmap f (Fst a) = Fst (f a)
  fmap f (Snd b) = Snd b


data Company a c b =
    DeepBlue a c
  | Something b
  deriving (Eq, Show)

instance Functor (Company e e') where
  fmap f (DeepBlue a c) = DeepBlue a c
  fmap f (Something b) = Something (f b)


data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b =
    Finance
  | Desk    a
  | Floor   b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Floor b) = Floor (f b)

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

func :: Num a => String -> String -> Integer -> [a]
func = undefined

newtype K a b = K a
  deriving (Eq, Show)

-- cool, but very hack -> very rarely do that in production
instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K (f x))


data Evil a b = Evil b
  deriving (Eq, Show)

instance Functor (Evil a) where
  fmap f (Evil b) = Evil (f b)

data LiftItOut f a = LiftItOut (f a)
  deriving (Eq, Show)

instance Functor functor => Functor (LiftItOut functor) where
  fmap f (LiftItOut ftor) = LiftItOut (fmap f ftor)

data Parappa f g a =
  Wrapper (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (Wrapper left right) = Wrapper (fmap f left) (fmap f right)

data IgnoreOne f g a b = IgnoreSomething (f a) (g b)
  deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething left right) = IgnoreSomething left (fmap f right)

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x rem) = Cons (f x) (fmap f rem)

data GoatLord a =
    NoGoat
  | Goat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap f NoGoat            = NoGoat
  fmap f (Goat a)          = Goat (f a)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- last
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print s x) = Print s (f x)
  fmap f (Read func) = Read (fmap f func)

-- done!



