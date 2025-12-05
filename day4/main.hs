module Main where

import Data.Array

type Grid = Array (Int, Int) Char

parseInput :: String -> Grid
parseInput input =
    let rows = lines input
        h = length rows
        w = length (head rows)
        bounds = ((0, 0), (h - 1, w - 1))
        assocList = [((r, c), char) | (r, row) <- zip [0..] rows, (c, char) <- zip [0..] row]
    in array bounds assocList

getNeighbors :: (Int, Int) -> [(Int, Int)]
getNeighbors (r, c) =
    [ (r + dr, c + dc)
    | dr <- [-1..1]
    , dc <- [-1..1]
    , not (dr == 0 && dc == 0)
    ]

countPaperNeighbors :: Grid -> (Int, Int) -> Int
countPaperNeighbors grid pos =
    length [ () | nPos <- getNeighbors pos, inRange (bounds grid) nPos, grid ! nPos == '@' ]

isAccessible :: Grid -> (Int, Int) -> Bool
isAccessible grid pos =
    (grid ! pos == '@') && (countPaperNeighbors grid pos < 4)

simulateRemoval :: Grid -> Int -> Int
simulateRemoval grid currentTotal =
    let
        removable = [ pos | pos <- indices grid, isAccessible grid pos ]
        count = length removable
    in
        if count == 0
        then currentTotal
        else
            let newGrid = grid // [(pos, '.') | pos <- removable]
            in simulateRemoval newGrid (currentTotal + count)

solve :: String -> Int
solve input =
    let grid = parseInput input
    in simulateRemoval grid 0

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print $ solve contents
