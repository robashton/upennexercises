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


-- TODO: Is it possible to do an infix data constructor?
data Stream a = Cons a (Stream a)

-- um, what does this actually mean?
instance Show a => Show (Stream a) where
  show x = show $ take 20 $ streamToList x

streamToList :: Stream a -> [a]
streamToList (Cons value next) = value : streamToList(next)

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons value next) = Cons (f value) (streamMap f next)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f v = Cons v (streamFromSeed f (f v))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons value next) ignored = Cons value (interleaveStreams ignored next)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) $ streamMap rulerValue $ streamFromSeed (+2) 2

rulerValue :: Integer -> Integer
rulerValue x = x

---- dumb way first
--ruler :: Stream Integer
--ruler = streamMap rulerValue nats


--1 2 3 4  5  6
--2 4 8 16 32 64
--
--
--1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
--0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4,0  1  0  4  0  1  0  8



