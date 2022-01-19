-- 1.
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

-----------------------------------------------------------------------------
-- 2.
q2 :: Integer
q2 = dodgy 1 1
--
-- Answer:
-- 11

-----------------------------------------------------------------------------
-- 3.
q3 :: Integer
q3 = dodgy 2 2
--
-- Answer:
-- 22

-----------------------------------------------------------------------------
-- 4.
q4 :: Integer
q4 = dodgy 1 2
--
-- Answer:
-- 21

-----------------------------------------------------------------------------
-- 5.
q5 :: Integer
q5 = dodgy 2 1
--
-- Answer:
-- 12

-----------------------------------------------------------------------------
-- 6.
q6 :: Integer
q6 = oneIsOne 1
--
-- Answer:
-- 11

-----------------------------------------------------------------------------
-- 7.
q7 :: Integer
q7 = oneIsOne 2
--
-- Answer:
-- 21

-----------------------------------------------------------------------------
-- 8.
q8 :: Integer
q8 = oneIsTwo 1
--
-- Answer:
-- 21

-----------------------------------------------------------------------------
-- 9.
q9 :: Integer
q9 = oneIsTwo 2
--
-- Answer:
-- 22

-----------------------------------------------------------------------------
-- 10.
q10 :: Integer
q10 = oneIsOne 3
--
-- Answer:
-- 31

-----------------------------------------------------------------------------
-- 11.
q11 :: Integer
q11 = oneIsTwo 3
--
-- Answer:
-- 23
