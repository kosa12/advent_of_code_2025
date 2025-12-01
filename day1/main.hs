module Main where

import System.IO

data Direction = L | R deriving (Show, Eq)

parseLine :: String -> (Direction, Int)
parseLine (d:n) = (dir, read n)
  where dir = if d == 'L' then L else R

processMove :: Int -> (Direction, Int) -> (Int, Int)
processMove current (R, dist) =
    let hits = (current + dist) `div` 100 - current `div` 100
        newPos = (current + dist) `mod` 100
    in (newPos, hits)
processMove current (L, dist) =
    let a = current - dist
        b = current - 1
        hits = (b `div` 100) - ((a - 1) `div` 100)
        newPos = (current - dist) `mod` 100
    in (newPos, hits)

solve :: [(Direction, Int)] -> Int
solve instructions =
    let startPos = 50
        (_, totalHits) = foldl step (startPos, 0) instructions
        step (pos, accHits) instr =
            let (newPos, hits) = processMove pos instr
            in (newPos, accHits + hits)
    in totalHits

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let linesOfFile = filter (not . null) $ lines contents
    let instructions = map parseLine linesOfFile
    print $ solve instructions
