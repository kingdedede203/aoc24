module Main where

import Control.Arrow (arr, (&&&))
import Control.Applicative (liftA2)

parse :: String -> [[Int]]
parse = map (map read . words) . lines

safeRecord :: [Int] -> Bool
-- safeRecord record = withinBounds (differences record) && sameSign (differences record)
-- withinBounds, sameSign :: Int a => ([a] ->) Bool
safeRecord = liftA2 (&&) withinBounds equalSign . differences  -- think of a more natural way to arrive at this
    where
        withinBounds = all (liftA2 (&&) (>=1) (<=3) . abs)
        equalSign (x:xs) = all ((==signum x) . signum) xs
        -- differences record = (zipWith (-) record) (tail record)
        differences = zipWith (-) <*> tail

p1 :: [[Int]] -> Int
p1 = length . filter safeRecord

p2 :: [[Int]] -> Int
p2 = length . filter canBeSafe
    where
        canBeSafe = any safeRecord . dampenRecord 
        dampenRecord [] = []
        dampenRecord (x:xs) = xs : map (x:) (dampenRecord xs)

main :: IO ()
main = do
    parsedInput <- parse <$> readFile "input.txt"
    print $ (arr p1 &&& arr p2) parsedInput
