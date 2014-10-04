module Golf where

import Data.List

-- Blah blah blah explanation
skip :: Int -> [a] -> [a]
skip n = map snd . filter ((==0) . (`mod` n) . fst) . zip [1..]

skips :: [a] -> [[a]]
skips xs = map (`skip` xs) [1..length(xs)]

-- Blah blah blah explanation (Cheeky rob is cheeky, drop 1 drop 2 CONCAT)
maxima :: (Integer,Integer, Integer) -> [Integer]
maxima (x,y,z) = if x < y && y > z then [y] else []

generate :: [Integer] -> [(Integer, Integer, Integer)]
generate xs = zip3 xs (drop 1 xs) (drop 2 xs)

localMaxima :: [Integer] -> [Integer]
localMaxima = concat . map maxima . generate

--- FUUUUUUUUUUUUUUUUUUUUUUU

printline :: Int -> [Int] -> String
printline line = map (\y -> if y > line then '*' else ' ')

printogram :: [Int] -> String
printogram xs = intercalate "\n" $ reverse $ "==========" : ['0'..'9'] : map (`printline` xs) [0..(maximum xs)]

histogram :: [Integer] -> String
histogram xs = printogram $ map (\n -> length $ filter (==n) xs) [0..9]

testicles :: [Integer] -> IO ()
testicles = putStr . histogram

