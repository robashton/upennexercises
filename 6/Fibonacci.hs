{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- We need an accumulator, I can see why I went for foldr..
-- To calculate the current n we merely need the last two ns
-- this could be a fold with 0 and 1 as a pair
-- this could actually be "iterate"

-- Wonder if there is a better way to define that iteration fn..?
fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(x,y) -> (y, (x+y))) (0,1)

data Stream a = a :> (Stream a)

-- um, what does this actually mean?
instance Show a => Show (Stream a) where
  show x = show $ take 20 $ streamToList x

streamToList :: Stream a -> [a]
streamToList (value :> next) = value : streamToList(next)

streamRepeat :: a -> Stream a
streamRepeat x = x :> (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (value :> next) = (f value) :> (streamMap f next)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f v = v :> (streamFromSeed f (f v))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (value :> next) ignored =  value :> (interleaveStreams ignored next)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) $ streamMap rulerValue $ streamFromSeed (+2) 2

-- doing it the dumb way cos I really can't work
-- out what the pattern is - I should stop doing haskell
-- when drunk on aeroplanes!!!

rulerValue :: Integer -> Integer
rulerValue x = rulerValueInner x where
  rulerValueInner y
    | y == 1 = 1
    | (x `mod` (2^y)) == 0 = y
    | otherwise = rulerValueInner $ y-1

-- -----------
-- Bonus points
-- -----------
-- Need to work out what he means by this and generating functions, next exercise
--x :: Stream Integer

instance Num (Stream Integer) where
  fromInteger x = streamRepeat x
  (+) x y = interleaveStreams x y
  (-) x y = interleaveStreams x y
  (*) x y = interleaveStreams x y





--1 2 3 4  5  6
--2 4 8 16 32 64
--
--
--1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
--0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4,0  1  0  4  0  1  0  8

