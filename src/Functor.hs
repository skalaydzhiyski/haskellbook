module Functor where

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

