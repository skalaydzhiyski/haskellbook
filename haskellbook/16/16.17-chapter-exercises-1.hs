import           GHC.Arr
import           Test.QuickCheck
import           Test.QuickCheck.Function


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c))
               => f a
               -> Fun a b
               -> Fun b c
               -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

-- Determine if the a valid `Functor` can be written theses datatypes.
--
-----------------------------------------------------------------------------
-- 1.
-- data Bool = False
--           | True
--
-- Answer:
-- Bool is a type constant with the kind `*`.

-----------------------------------------------------------------------------
-- 2.
data BoolAndSomethingElse a = False' a
                            | True' a
                            deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' x) = False' (f x)
  fmap f (True' x)  = True' (f x)

instance Arbitrary a => Arbitrary (BoolAndSomethingElse a) where
  arbitrary = do
    x <- arbitrary
    elements [ False' x
             , True' x
             ]

type BoolEtcId = BoolAndSomethingElse Int -> Bool
type BoolEtcFC = BoolAndSomethingElse Int -> IntToInt -> IntToInt -> Bool

-----------------------------------------------------------------------------
-- 3.
data BoolAndMaybeSomethingElse a = Falsish
                                 | Truish a
                                 deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish    = Falsish
  fmap f (Truish x) = Truish (f x)

instance Arbitrary a => Arbitrary (BoolAndMaybeSomethingElse a) where
  arbitrary = do
    x <- arbitrary
    elements [ Falsish
             , Truish x
             ]

type BoolMaybeEtcId = BoolAndMaybeSomethingElse Int -> Bool
type BoolMaybeEtcFC = BoolAndMaybeSomethingElse Int
                   -> IntToInt
                   -> IntToInt
                   -> Bool


-----------------------------------------------------------------------------
-- 4.
newtype Mu f = InF
             { outF :: f (Mu f) }

-- Answer:
-- Mu has type `(* -> *) -> *` with `f` being a higher kinded type.


-----------------------------------------------------------------------------
-- 5.
data D = D (Array Word Word) Int Int
  deriving (Eq, Show)

-- Answer:
-- No


main :: IO ()
main = do
  putStrLn "\n    BoolAndSomethingElse"
  quickCheck (functorIdentity :: BoolEtcId)
  quickCheck (functorCompose  :: BoolEtcFC)
  putStrLn "\n    BoolAndMaybeSomethingElse"
  quickCheck (functorIdentity :: BoolMaybeEtcId)
  quickCheck (functorCompose  :: BoolMaybeEtcFC)

