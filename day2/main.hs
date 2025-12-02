module Main where

import Data.Char (isDigit)
import qualified Data.Set as Set
import Data.List (sort)

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

generateInvalid :: Integer -> [Integer]
generateInvalid maxVal =
    let
        maxDigits = length (show maxVal)
        maxL = maxDigits `div` 2

        genForBaseLen len =
            let start = 10^(len-1)
                end = 10^len - 1

                m2 = 1 + 10^len
                powers = map (\k -> 10^(k*len)) [2..]
                ms = scanl (+) m2 powers

                multipliers = takeWhile (\m -> start * m <= maxVal) ms

            in [ val | b <- [start..end], m <- multipliers, let val = b * m, val <= maxVal ]

        allInvalids = concatMap genForBaseLen [1..maxL]
    in
        Set.toAscList $ Set.fromList allInvalids

solve :: String -> Integer
solve input =
    let
        cleanInput = filter (\c -> c /= '\n' && c /= ' ') input
        rangesStr = filter (not . null) $ split ',' cleanInput
        ranges = map parseRange rangesStr

        maxLimit = maximum (map snd ranges)
        invalids = generateInvalid maxLimit

        sumForRange (start, end) =
            sum $ takeWhile (<= end) $ dropWhile (< start) invalids
    in
        sum (map sumForRange ranges)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print $ solve contents
