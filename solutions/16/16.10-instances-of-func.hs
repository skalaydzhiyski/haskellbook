{-# LANGUAGE ViewPatterns #-}

import           Test.QuickCheck
import           Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c))
               => (a -> b)
               -> (b -> c)
               -> f a
               -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Functor f, Eq (f c))
                => f a
                -> Fun a b
                -> Fun b c
                -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

-----------------------------------------------------------------------------
-- 1.
newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

type IdId = Identity Int -> Bool
type IdFC = Identity Int -> IntToInt -> IntToInt -> Bool

-----------------------------------------------------------------------------
-- 2.
data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    return $ Pair x x

type PairId = Pair Int -> Bool
type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool

-----------------------------------------------------------------------------
-- 3.
data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

type TwoId = Two Int Int -> Bool
type TwoFC = Two Int Int -> IntToInt -> IntToInt -> Bool

-----------------------------------------------------------------------------
-- 4.
data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         ) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

type ThreeId = Three Int Int Int -> Bool
type ThreeFC = Three Int Int Int -> IntToInt -> IntToInt -> Bool

-----------------------------------------------------------------------------
-- 5.
data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y y') = Three' x (f y) (f y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Three' x y y

type Three'Id = Three' Int Int -> Bool
type Three'FC = Three' Int Int -> IntToInt -> IntToInt -> Bool


-----------------------------------------------------------------------------
-- 6.
data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d
         ) => Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four w x y z

type FourId = Four Int Int Int Int -> Bool
type FourFC = Four Int Int Int Int -> IntToInt -> IntToInt -> Bool

-----------------------------------------------------------------------------
-- 7.
data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x x' x'' y) = Four' x x' x'' (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Four' x x x y

type Four'Id = Four' Int Int -> Bool
type Four'FC = Four' Int Int -> IntToInt -> IntToInt -> Bool

-----------------------------------------------------------------------------
-- 8.
-- Can you implement one for this type?
data Trivial = Trivial

-- Answer:
-- `Trivial` is type constant with the kind `*`
-- `Functor` requires an instance has kind `* -> *`

-----------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "\n 1. Identity"
  quickCheck (functorIdentity :: IdId)
  quickCheck (functorCompose' :: IdFC)
  putStrLn "\n 2. Pair"
  quickCheck (functorIdentity :: PairId)
  quickCheck (functorCompose' :: PairFC)
  putStrLn "\n 3. Two"
  quickCheck (functorIdentity :: TwoId)
  quickCheck (functorCompose' :: TwoFC)
  putStrLn "\n 4. Three"
  quickCheck (functorIdentity :: ThreeId)
  quickCheck (functorCompose' :: ThreeFC)
  putStrLn "\n 5. Three'"
  quickCheck (functorIdentity :: Three'Id)
  quickCheck (functorCompose' :: Three'FC)
  putStrLn "\n 6. Four"
  quickCheck (functorIdentity :: FourId)
  quickCheck (functorCompose' :: FourFC)
  putStrLn "\n 7. Four'"
  quickCheck (functorIdentity :: Four'Id)
  quickCheck (functorCompose' :: Four'FC)
