module Main where

import Data.List (transpose)
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)

splitIntoBlocks :: [String] -> [[String]]
splitIntoBlocks rows =
    let
        cols = transpose rows

        groupedCols = groupBySeparator cols

        blocks = map transpose groupedCols
    in blocks
  
groupBySeparator :: [String] -> [[String]]
groupBySeparator [] = []
groupBySeparator (col:rest)
    | all (== ' ') col = groupBySeparator rest
    | otherwise =
        let (block, remaining) = break (all (== ' ')) (col:rest)
        in block : groupBySeparator remaining

solveBlock :: [String] -> Integer
solveBlock block =
    let 
        nonEmptyLines = filter (not . all (== ' ')) block

        operatorLine = last nonEmptyLines
        operatorChar = head $ filter (/= ' ') operatorLine

        digitRows = init nonEmptyLines
        cols = transpose digitRows

        parseColumn :: String -> Maybe Integer
        parseColumn s =
            let digits = filter isDigit s
            in if null digits then Nothing else Just (read digits)

        numbers = mapMaybe parseColumn cols

        op = case operatorChar of
            '+' -> (+)
            '*' -> (*)

    in foldl1 op numbers

solve :: String -> Integer
solve input =
    let
        rows = lines input
        maxLen = maximum (map length rows)
        paddedRows = map (\r -> r ++ replicate (maxLen - length r) ' ') rows

        blocks = splitIntoBlocks paddedRows
        results = map solveBlock blocks
    in sum results

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print $ solve contents
