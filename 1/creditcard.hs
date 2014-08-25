toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
  | n < 0 = []
  | otherwise = rem n 10 : (toDigitsRev $ div n 10)

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
  | n < 0 = []
  | otherwise = toDigits (div n 10) ++ [rem n 10]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:ys)) = [x, y*2] ++ doubleEveryOther ys

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:xs) = (sumDigits . toDigits) x + sumDigits xs

validate :: Integer -> Bool
validate n
 | (sumDigits (doubleEveryOther (toDigitsRev n))) `rem` 10 == 0 = True
 | otherwise = False
