module Main where

import Data.List (sortBy)
import Data.Ord (comparing)

split :: Char -> String -> [String]
split delimiter str = case break (== delimiter) str of
    (a, rest) ->
        if null rest
        then [a]
        else a : split delimiter (tail rest)

parseRange :: String -> (Integer, Integer)
parseRange s = 
    let [a, b] = split '-' s
    in (read a, read b)

mergeRanges :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeRanges [] = []
mergeRanges ranges =
    let sorted = sortBy (comparing fst) ranges
    in foldl merge [] sorted
  where
    merge [] r = [r]
    merge acc@(prev:rest) (lo, hi)
        | lo <= snd prev + 1 = (fst prev, max (snd prev) hi) : rest
        | otherwise = (lo, hi) : acc

countFreshIds :: [(Integer, Integer)] -> Integer
countFreshIds = sum . map (\(lo, hi) -> hi - lo + 1)

solve :: String -> Integer
solve input =
    let linesOfFile = lines input
        (rangeLines, _) = break null linesOfFile

        ranges = map parseRange rangeLines
        mergedRanges = mergeRanges ranges
    in
        countFreshIds mergedRanges

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print $ solve contents
