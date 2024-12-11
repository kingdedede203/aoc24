module Main where

import Control.Arrow (arr, (&&&))
import Data.List (sort)

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

parse :: String -> ([Int], [Int])
parse = unzip . map (toInt . words) . lines
    where toInt [x, y] = (read x, read y)

p1 :: ([Int], [Int]) -> Int
p1 (xs, ys) = sum $ zipWith (abs ... (-)) (sort xs) (sort ys)

p2 :: ([Int], [Int]) -> Int
p2 (xs, ys) = foldl (\acc x -> acc + x * length (filter (x==) ys)) 0 xs

main :: IO ()
main = do
    parsedInput <- parse <$> readFile "input.txt"
    print $ (arr p1 &&& arr p2) parsedInput
