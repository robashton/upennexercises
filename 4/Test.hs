module Test where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)


-- I don't think he even wants those guard clauses?
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate nextfn
  where nextfn x
          | even x = x `div` 2
          | otherwise = 3 * x + 1


-- Wow yuck, there has to be a tidier way of composing this
data Tree a = Leaf
            | Node Int (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree xs = foldr insert Leaf xs
  where insert :: a -> Tree a -> Tree a
        insert x Leaf = Node 0 Leaf x Leaf
        insert x (Node height left val right)
          | treeHeight left <= treeHeight right =
            let newLeft = insert x left
           in (Node ((+1) $ treeHeight newLeft) newLeft val right)
          | otherwise  =
            let newRight = insert x right
            in (Node height left val newRight)

treeHeight :: Tree a -> Int
treeHeight Leaf = 0
treeHeight (Node height _ _ _) = height


-- Ah, this is good
xor :: [Bool] -> Bool
xor = even . length . filter not

-- This one is straightfoward at least
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- And this one
-- I thought about using a fold with a pair to only iterate through once
-- but overkill
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ filter (flip notElem $ sieveRange n) [1..n]

sieveRange :: Integer -> [Integer]
sieveRange n = filter (<n) . map (\(i,j) -> i + j + 2 * (i * j)) $ crossRange n

crossRange :: Integer -> [(Integer, Integer)]
crossRange n = [(i,j) | i <- [1..n], j <- [1..n]]


