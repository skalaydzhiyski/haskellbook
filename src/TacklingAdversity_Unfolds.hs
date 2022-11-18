module TacklingAdversity_Unfolds where

import Data.List


notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

replace_ :: Maybe String -> String
replace_ Nothing = "a"
replace_ (Just x)  = x

replaceThe :: String -> String
replaceThe s = intercalate " " $ map (replace_ . notThe) $ words s

-- +

newtype Word' = Word' String deriving (Show, Eq)

vowels = "aeiou"

validate :: String -> Maybe Word'
validate s = if vs > cs then Nothing else Just (Word' s)
  where vs = sum $ map (\x -> if elem x vowels then 1 else 0) s
        cs = sum $ map (\x -> if elem x vowels then 0 else 1) s

mkWord :: String -> Word'
mkWord s = case validate s of
             Nothing -> Word' ""
             Just x  -> x
-- +

data Nat =  Zero
          | Succ Nat
          deriving (Show, Eq)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

-- +

isJust :: Maybe a -> Bool
isJust (Just x) = True
_             = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
_             = False


maybe_ :: b -> (a -> b) -> Maybe a -> b
maybe_ x _ Nothing  = x
maybe_ _ f (Just x) = f x

fromMaybe_ :: a -> Maybe a -> a
fromMaybe_ d Nothing  = d
fromMaybe_ _ (Just x) = x

catMaybes :: [Maybe a] -> [a]
catMaybes []     = []
catMaybes (x:xs) = case x of
                     Nothing -> catMaybes xs
                     Just x  -> x : catMaybes xs

{-

foldr (-) 0 [1,2,3]
1 - (2 - (3 - 0))

go (Just left) (Just right) = Just ([left] ++ right)
foldr go (Just []) [Just 1, Just 2, Just 3]

Just ([1] ++ (Just ([2] ++ (Just ([3] ++ [])))))
NOTE: at every step the `go` function expands the Just through pattern matching and gets the inner list.

-}
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr go (Just [])
  where go Nothing _ = Nothing
        go _ Nothing = Nothing
        go (Just left) (Just right) = Just ([left] ++ right)

-- +

lefts' :: [Either a b] -> [a]
lefts' = foldr (\x y -> case x of
                          Left l  -> l:y
                          Right _ -> y) []

rights' :: [Either a b] -> [b]
rights' = foldr (\x y -> case x of
                          Right l -> l:y
                          Left _  -> y) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' lst = (lefts' lst, rights' lst)

-- Second solution to the same problem to illustrate the power of pattern matching.
partitionEithers'' :: [Either a b] -> ([a], [b])
partitionEithers'' = foldr go ([],[])
  where go (Left x) (l,r)  = (x:l,r)
        go (Right x) (l,r) = (l,x:r)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just (f x)
eitherMaybe' _ _ = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left x)  = f x
either' f g (Right x) = g x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (\_ -> Nothing) (\x -> Just $ f x) x

-- +

iterate_ :: (a -> a) -> a -> [a]
iterate_ f x = x : iterate_ f (f x)

unfoldr_ :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr_ f x = case f x of
                 Nothing -> []
                 Just (l,r) -> l : unfoldr_ f r

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = unfoldr_ (\x -> Just (x, f x)) x

-- +

data Tree a =
    Leaf
  | Node (Tree a) a (Tree a)
  deriving (Show, Ord, Eq)

-- it works, but don't know how to test :)
unfold :: (a -> Maybe (a, b, a)) -> a -> Tree b
unfold f x = case f x of
               Nothing      -> Leaf
               Just (l,v,r) -> Node (unfold f l) v (unfold f r)

-- now for something omre sensible (n = tree height)
treeBuild :: Integer -> Tree Integer
treeBuild 0 = Leaf
treeBuild n = Node (treeBuild $ n-1) n (treeBuild $ n-1)

