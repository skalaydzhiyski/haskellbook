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


-- continue from here after the interview
