module Main where

import Data.Char (digitToInt)
import Data.List (elemIndex)

findMaxSubseq :: Int -> String -> String
findMaxSubseq 0 _ = ""
findMaxSubseq k s =
    let n = length s
        searchLen = n - k + 1
        searchSpace = take searchLen s
        maxDigit = maximum searchSpace

        Just idx = elemIndex maxDigit searchSpace
    in maxDigit : findMaxSubseq (k - 1) (drop (idx + 1) s)

solveLine :: String -> Integer
solveLine line = read (findMaxSubseq 12 line)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let linesOfFile = filter (not . null) $ lines contents
    let result = sum $ map solveLine linesOfFile
    print result
