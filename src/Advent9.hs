{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Advent9
    ( advent9_1, advent9_2
    ) where

import Data.List (nub)
import Text.Heredoc
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator (manyTill)

-- Answers

advent9_1 = length . nub . map (snd) . traverseRope . directionsList <$> parse parseInput "" input

advent9_2 = traverseRope . directionsList <$> parse parseInput "" input_example

-- Traverse rope

traverseRope = scanl (moveRope) startingRope

startingRope = ((0,0),(0,0))

-- List of intructions to directions

directionsList = concatMap (\(d, n) -> take n $ repeat d)

-- Rope behaviour

moveRope :: ((Int, Int), (Int, Int)) -> Direction -> ((Int, Int), (Int, Int))
moveRope ((xHead,yHead), posTail) direction = (newHeadPos, moveTail newHeadPos posTail direction) where
    newHeadPos = case (direction) of
        U -> (xHead, yHead - 1)
        D -> (xHead, yHead + 1)
        R -> (xHead + 1, yHead)
        L -> (xHead - 1, yHead)

moveTail h@(xHead, yHead) t@(xTail, yTail) d
    | headTailTogether h t = (xTail, yTail)
    | distanceX h t < (-1) = (xHead + 1, yHead)
    | distanceX h t >   1  = (xHead - 1, yHead)
    | distanceY h t < (-1) = (xHead, yHead + 1)
    | distanceY h t >   1  = (xHead, yHead - 1)

headTailTogether h t = ((abs $ distanceX h t) <= 1) && ((abs $ distanceY h t) <= 1)
distanceX (xHead, _) (xTail, _) = xHead - xTail
distanceY (_, yHead) (_, yTail) = yHead - yTail

-- Parse

parseInput = parseInstruction `sepBy` endOfLine

parseInstruction = do
    instruction <- upper
    space
    num <- many1 digit
    return (read [instruction] :: Direction, read num :: Int)

data Direction = U | D | L | R deriving (Show, Read, Eq)

-- Input

input_example = [here|R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2|]

input = [here|R 2
D 2
U 2
D 1
L 1
D 2
R 2
L 2
R 1
D 2
L 1
U 2
R 1
D 1
R 1
L 1
D 1
R 2
U 2
L 2
R 2
U 2
R 1
L 1
D 2
R 1
D 1
R 1
D 1
R 1
L 2
R 1
D 1
L 1
U 2
L 1
D 1
L 1
R 1
U 2
D 1
U 2
L 2
R 2
D 1
L 1
R 1
D 1
L 1
U 1
R 2
D 1
R 1
D 2
L 1
D 2
L 1
D 2
R 1
L 1
D 2
L 1
R 1
D 2
L 2
R 2
L 1
U 2
R 1
U 2
R 2
U 1
D 1
U 2
L 1
D 2
L 2
R 2
L 1
R 1
U 2
D 1
R 1
L 2
U 1
D 2
U 1
L 2
R 2
D 1
U 1
L 2
U 2
L 1
R 1
D 1
R 2
D 1
R 1
D 2
R 1
D 1
R 1
D 2
L 2
U 2
L 1
D 1
L 1
R 2
U 1
D 1
U 2
L 2
D 3
L 3
U 3
R 1
D 3
R 1
D 3
R 2
D 2
U 3
D 2
L 3
R 3
U 3
R 1
U 3
D 3
L 1
D 2
U 1
R 2
L 2
R 3
L 2
D 3
L 2
R 1
D 1
U 1
D 2
R 2
L 1
U 1
R 3
U 2
D 3
L 3
U 2
L 3
R 2
D 2
U 2
R 1
U 3
R 2
D 1
U 2
D 3
U 2
R 3
U 2
L 3
U 3
D 2
R 3
U 3
L 3
U 2
L 1
D 3
L 2
D 3
R 3
L 2
R 3
D 1
U 1
L 3
R 3
D 2
L 3
U 2
R 2
U 1
R 3
D 3
R 1
U 2
L 2
U 1
R 1
U 1
D 2
L 2
R 1
D 3
R 1
D 3
U 1
R 2
U 2
L 2
U 2
L 3
D 3
U 2
L 3
U 1
L 2
D 1
U 3
L 2
R 3
D 1
R 3
L 3
U 1
L 3
R 2
D 3
U 3
L 3
D 4
L 1
U 1
R 3
L 1
D 4
U 4
L 4
D 4
U 1
D 3
R 1
D 4
L 1
U 2
D 1
R 3
L 4
D 2
U 2
R 1
L 4
U 4
R 4
D 1
R 3
D 1
R 4
U 4
D 1
U 3
D 4
L 3
R 2
D 4
L 2
U 4
D 3
U 4
D 2
U 2
L 3
R 4
L 4
U 2
L 1
U 4
D 4
R 4
U 4
R 2
U 4
R 1
U 3
R 3
U 4
R 4
U 2
L 1
U 3
D 4
L 1
R 1
L 2
R 2
D 2
U 3
R 3
L 1
D 2
R 4
D 1
R 4
L 2
U 4
D 2
R 4
U 2
D 4
R 1
U 3
L 3
D 2
R 2
U 1
L 4
R 1
U 2
R 4
D 3
R 1
L 2
U 1
R 2
L 2
U 2
R 3
L 2
D 4
U 4
L 1
U 2
D 1
U 2
D 1
L 2
R 4
L 5
D 2
U 5
L 1
R 2
L 3
U 3
D 3
L 1
D 1
U 4
L 4
U 3
D 5
R 1
L 4
U 1
L 2
R 4
D 1
L 4
D 1
U 4
D 1
L 2
D 5
L 5
D 4
R 3
U 2
L 4
U 4
L 4
R 5
D 1
L 2
R 5
U 4
D 5
L 2
U 4
D 1
L 3
R 5
D 4
L 1
D 3
L 3
U 2
R 2
D 4
L 4
U 1
D 2
L 5
D 2
L 2
U 3
R 3
U 3
D 2
U 4
R 3
L 3
D 2
L 2
R 3
D 2
L 3
R 4
U 4
R 4
D 2
R 1
D 1
L 4
R 4
D 3
R 2
U 4
L 2
D 3
U 4
L 3
R 5
L 2
R 1
D 4
L 5
D 2
U 5
L 4
U 2
R 3
L 4
R 1
U 3
R 3
L 2
U 2
L 3
R 5
L 3
R 3
U 3
L 3
U 5
D 4
U 5
D 4
L 4
D 2
R 5
L 3
U 2
D 3
L 4
R 1
D 1
R 5
L 3
R 2
L 3
U 3
D 1
U 5
R 2
L 2
D 5
R 3
U 4
R 3
L 1
R 6
L 4
D 4
U 4
R 4
U 2
R 4
D 1
U 3
R 1
U 5
D 3
L 4
R 5
L 3
U 6
L 5
D 3
R 1
D 2
U 5
L 1
R 5
U 4
R 1
U 1
R 3
L 2
D 5
U 4
L 1
U 2
R 3
L 5
R 2
D 3
U 3
D 6
R 1
D 2
L 3
R 3
L 3
U 4
R 4
U 4
R 6
U 1
L 6
R 6
L 4
U 6
L 2
D 1
R 5
L 5
U 3
D 6
U 5
L 4
U 2
R 1
L 3
D 6
U 5
R 4
U 2
D 3
R 4
L 1
D 4
U 6
R 1
D 6
U 2
D 5
U 4
L 4
R 6
D 2
R 6
L 1
R 3
D 4
R 5
L 6
U 5
L 3
R 6
U 1
L 3
D 7
U 2
D 5
R 5
D 7
R 6
D 6
R 7
D 7
L 3
R 4
L 1
U 6
R 2
L 7
U 7
D 1
R 1
D 1
U 5
R 4
U 2
D 5
U 6
L 2
R 1
U 5
L 1
U 7
R 1
D 2
L 4
D 4
L 5
U 4
L 2
D 5
R 7
D 7
R 5
L 3
U 7
R 7
D 1
L 2
U 3
L 2
U 7
L 2
U 3
L 6
D 6
L 2
D 6
L 6
U 3
L 3
U 7
D 3
L 4
U 5
D 5
L 4
D 7
U 3
R 7
D 5
R 3
L 6
U 2
L 2
U 1
L 1
U 4
D 7
L 7
D 2
U 6
L 2
U 2
L 4
R 3
L 6
R 2
D 7
L 6
D 3
L 6
R 1
U 4
R 6
L 7
D 7
R 4
U 7
L 6
R 3
D 1
U 3
D 2
U 3
R 2
L 7
U 1
R 4
D 4
U 2
R 5
D 5
R 4
L 3
U 1
R 7
L 5
D 4
L 3
D 7
U 2
D 4
U 4
L 1
D 4
R 7
L 6
U 3
L 4
D 5
U 1
D 1
L 6
R 7
D 1
L 4
D 1
L 5
R 8
D 4
L 8
R 4
D 4
U 3
L 5
R 5
L 8
U 4
R 2
D 6
R 5
L 1
U 8
L 1
D 5
U 5
D 4
L 6
U 6
D 8
R 6
D 3
U 7
R 8
D 7
L 2
U 4
D 4
L 2
R 2
U 5
L 4
R 3
L 2
U 5
D 8
L 8
D 3
U 4
D 2
L 2
U 4
R 5
L 6
U 4
R 5
U 4
L 7
D 8
R 8
D 7
R 5
D 6
U 2
R 1
D 3
L 6
U 3
D 5
U 6
L 7
R 1
L 4
R 4
L 4
R 3
L 4
U 3
D 7
U 8
D 4
R 6
L 7
R 2
L 2
U 7
D 2
R 1
D 5
U 1
D 7
R 2
L 8
R 8
U 4
R 9
L 4
R 5
L 2
D 6
L 9
U 2
R 5
D 4
R 9
U 4
D 8
R 9
U 5
D 4
U 4
L 4
U 3
R 5
L 8
D 9
U 2
L 8
U 5
D 4
L 1
D 4
U 3
D 7
U 5
D 5
R 9
L 2
D 5
L 7
D 1
L 2
R 6
D 9
U 1
L 6
R 2
D 2
R 5
D 8
R 2
D 3
L 4
D 5
R 2
D 8
L 3
D 6
U 2
L 4
D 9
L 6
R 5
L 7
U 2
D 8
U 5
L 6
U 1
L 9
R 9
D 1
R 3
U 9
L 2
U 8
R 8
D 8
R 7
U 7
R 6
L 2
U 4
D 2
L 4
U 4
D 2
R 8
L 6
R 9
D 9
L 6
D 4
L 5
U 8
D 2
R 6
D 7
U 3
R 6
L 9
U 6
L 8
U 8
D 6
U 9
D 5
U 8
L 6
R 6
L 6
R 9
L 2
U 4
L 4
U 1
D 4
R 1
U 7
D 5
R 4
U 4
D 5
L 9
R 2
U 5
L 2
R 10
L 5
R 1
L 2
U 3
D 2
R 7
L 2
R 6
L 3
R 6
U 7
L 1
U 6
R 6
U 2
L 3
R 3
L 1
R 9
L 5
U 9
L 9
D 9
U 4
D 2
R 6
D 6
R 8
D 2
U 6
L 5
R 2
U 10
L 3
D 6
R 3
D 6
U 2
D 7
R 1
U 7
D 2
R 8
L 5
U 4
L 5
R 6
U 6
D 1
L 5
D 5
R 5
D 4
L 5
U 3
R 4
L 5
R 2
L 5
D 10
R 4
L 2
R 4
L 7
U 9
R 3
D 6
R 7
L 10
D 7
U 4
L 9
D 6
L 7
R 3
U 7
R 5
U 6
L 1
U 2
L 5
D 6
R 8
L 7
R 7
D 1
U 9
R 1
L 4
U 4
R 1
L 3
D 2
L 8
D 10
L 9
D 8
R 5
D 6
L 7
D 2
U 11
D 2
U 6
L 3
U 8
D 11
U 9
D 9
R 11
U 11
L 8
U 11
R 11
U 2
D 6
R 11
D 7
L 6
U 5
R 1
L 3
D 11
U 3
D 5
R 4
U 4
D 8
L 6
U 1
R 10
L 5
D 3
U 8
R 7
L 8
R 2
U 6
D 9
R 1
D 1
R 3
U 3
L 10
U 7
R 1
D 3
U 6
L 10
R 6
L 6
R 4
L 2
R 10
L 11
U 5
R 5
U 5
R 2
D 10
L 8
U 9
D 11
R 10
D 11
R 11
D 2
U 7
R 11
U 2
L 9
R 11
U 1
R 8
L 5
U 6
D 5
L 10
U 1
L 4
R 7
L 5
D 2
R 9
D 10
U 4
R 1
D 7
R 10
D 10
L 5
R 3
L 8
R 8
U 10
R 9
D 11
R 1
D 9
R 5
L 3
D 5
L 8
U 8
R 10
D 10
U 1
D 3
R 6
U 1
L 2
U 6
L 7
R 11
U 2
R 2
L 5
U 5
R 2
D 12
U 3
L 2
D 5
U 3
R 11
D 5
R 10
U 8
L 7
R 6
D 2
U 11
L 10
R 12
D 8
U 12
L 7
D 5
R 11
U 10
R 12
U 1
L 1
U 10
L 2
R 9
U 1
R 4
U 2
R 8
D 1
R 2
L 3
R 7
D 7
L 10
R 2
U 11
R 8
U 11
D 12
U 8
L 5
U 12
L 12
U 12
L 7
R 5
D 4
U 9
L 5
R 7
L 10
R 4
L 11
D 3
R 1
D 12
U 10
R 10
D 5
R 11
U 2
L 8
R 4
D 1
L 10
R 2
D 11
L 4
U 12
D 11
L 11
U 3
L 9
U 8
D 8
R 10
L 8
U 2
D 9
U 12
D 7
U 8
L 8
D 9
L 6
U 8
D 11
U 12
D 9
L 1
D 4
R 2
U 10
D 2
L 10
U 6
L 9
D 4
U 4
R 4
L 5
R 9
U 2
R 5
U 4
D 12
R 3
L 10
U 7
D 1
U 9
R 6
L 4
U 9
L 13
D 2
L 8
R 12
D 7
U 13
D 6
U 12
L 4
R 4
D 10
U 8
L 4
R 4
D 2
U 9
L 9
D 11
L 6
R 2
L 8
R 9
L 6
U 13
R 10
D 3
U 10
L 12
R 9
U 12
R 10
U 10
D 11
L 7
R 5
L 13
U 11
R 13
U 11
R 12
U 8
R 12
D 4
L 10
R 1
L 9
R 13
L 2
U 7
L 13
R 4
U 13
L 6
R 12
D 1
U 7
L 10
R 6
D 6
U 2
R 2
L 8
D 5
U 9
D 5
L 9
R 1
U 5
L 13
D 1
R 5
L 8
U 5
R 2
U 8
L 1
U 13
R 6
U 12
L 3
D 1
U 4
L 12
R 1
D 6
L 11
U 7
D 1
L 9
R 4
U 7
D 7
U 9
D 1
L 10
D 14
U 3
D 9
L 7
U 1
L 9
R 13
L 14
R 11
D 8
L 12
U 11
R 11
U 9
R 2
U 13
R 5
D 3
U 3
R 9
D 14
U 2
D 8
L 7
D 2
U 8
L 12
U 7
D 12
R 11
D 14
R 13
L 13
R 9
L 7
U 4
D 9
U 9
L 12
U 3
D 4
R 5
D 14
L 9
D 14
U 3
L 8
R 1
U 2
R 14
D 5
L 6
R 9
D 12
R 3
D 1
L 8
U 11
D 11
L 7
R 14
D 9
L 9
R 13
L 4
D 11
R 14
U 6
L 10
R 7
D 9
U 7
D 5
R 8
U 2
R 9
D 6
R 13
U 1
L 14
R 6
L 9
D 11
L 3
U 5
R 12
U 8
L 3
D 5
U 7
R 2
D 14
R 14
L 2
U 5
L 11
U 7
L 14
U 5
D 6
R 14
D 12
U 4
D 5
R 14
L 14
D 12
L 10
D 2
L 1
R 5
D 8
R 2
L 14
R 5
L 12
D 9
L 6
D 8
L 8
R 12
U 1
L 11
U 3
D 7
R 11
D 15
L 4
U 9
R 15
U 8
D 4
L 10
U 2
L 13
U 9
L 5
D 13
L 5
R 1
D 3
U 11
D 5
L 13
R 3
D 4
U 3
R 5
U 14
R 4
D 9
R 3
D 11
U 13
R 7
U 10
R 1
L 9
D 15
R 6
U 12
L 4
R 13
U 7
D 10
R 13
U 1
R 4
D 15
L 10
R 11
U 9
L 1
D 6
R 15
U 10
L 10
D 6
U 10
L 8
R 5
L 14
D 6
L 2
U 2
L 9
U 2
L 7
U 9
L 2
D 8
R 6
D 6
R 7
U 2
L 13
R 13
U 3
D 13
U 14
R 9
L 4
U 15
R 1
D 2
U 15
R 12
U 12
L 12
U 9
R 8
U 3
L 7
D 3
U 2
R 7
D 4
R 6
U 13
D 7
U 7
D 8
R 13
U 10
D 16
L 12
U 8
R 6
U 10
L 14
D 12
R 2
L 9
U 8
D 7
U 6
R 7
L 14
D 6
L 10
U 11
L 3
D 6
R 2
U 8
L 2
D 10
R 16
U 14
R 10
D 13
R 3
D 8
R 2
D 16
L 12
D 13
U 6
D 2
L 4
R 14
D 16
U 12
L 1
D 9
L 7
U 1
L 3
U 7
R 7
D 3
L 11
R 9
D 5
R 10
D 12
R 1
U 8
L 8
D 5
R 12
U 8
R 11
U 3
D 5
R 9
L 11
U 14
L 14
D 5
U 12
L 9
D 2
U 6
L 7
D 6
L 2
R 2
D 11
R 13
D 10
U 4
L 13
U 5
L 6
R 3
U 16
D 13
R 1
U 2
D 4
R 3
U 10
D 16
L 1
U 1
R 16
D 6
U 7
D 3
R 4
U 2
D 4
L 8
U 9
R 8
U 15
D 4
R 11
D 4
R 12
U 1
D 2
R 4
D 17
L 4
U 1
D 1
L 3
R 16
D 12
R 6
U 3
R 4
U 2
D 8
R 5
D 4
R 1
U 5
L 12
D 1
R 12
U 3
D 13
L 13
U 11
D 8
R 3
D 14
L 10
D 1
L 4
D 4
R 9
L 10
U 6
L 8
R 7
L 2
R 1
L 4
D 8
R 14
U 8
L 13
D 12
R 17
D 2
R 5
U 12
R 8
D 16
L 6
R 14
D 9
U 7
L 5
U 14
L 10
U 5
L 4
U 7
R 17
D 1
U 6
L 12
D 10
L 2
D 17
L 14
R 3
D 6
U 11
R 14
L 11
D 8
U 1
R 1
L 14
U 17
L 15
U 3
R 5
D 3
L 12
R 10
U 17
R 7
D 2
U 16
D 10
L 12
R 5
D 10
L 14
D 17
U 4
L 9
D 11
R 5
D 4
U 5
L 16
D 16
L 5
U 10
D 8
L 14
U 11
L 17
D 1
U 10
R 9
U 17
L 8
R 11
L 15
D 9
U 18
D 3
R 4
L 7
R 2
U 5
L 17
U 2
R 11
L 11
U 7
D 17
U 11
R 5
D 14
U 7
R 17
D 10
U 18
D 15
L 7
U 2
D 3
U 14
R 6
D 2
L 5
R 16
D 8
U 2
D 16
R 9
U 12
R 4
D 14
R 10
U 1
R 1
L 8
D 15
L 8
D 16
U 9
R 11
L 17
U 18
L 2
R 4
L 1
U 9
L 13
R 1
U 17
L 12
R 3
D 15
R 3
U 3
D 8
L 6
R 4
D 13
U 2
R 1
L 10
U 9
R 2
D 16
R 9
U 15
R 10
U 4
R 6
D 10
U 4
R 11
D 1
R 10
U 17
R 4
D 9
U 17
R 7
U 11
L 10
R 10
L 10
U 8
L 18
U 14
R 16
L 12
U 18
R 1
D 4
L 9
R 18
D 12
L 17
D 12
U 4
D 1
U 16
R 17
U 3
D 13
L 14
D 9
L 19
U 13
L 2
U 9
D 7
U 7
D 4
L 10
R 6
D 11
R 7
U 10
D 13
U 18
L 14
D 7
L 4
R 11
D 8
L 8
R 9
L 2
U 11
R 13
L 13
R 11
L 16
D 10
L 6
R 3
D 8
L 19
U 18
L 14
D 15
U 10
D 11
R 19
D 14
U 2
R 7
U 6
D 15
U 10
D 4
U 10
L 9
R 15
L 8
U 7
R 4
D 5
R 18
L 3
R 11
D 11
R 2
U 1
L 6
D 18
R 15
U 2
R 10
L 18
R 1
U 19
D 15
R 18
L 18
R 4
L 7
D 18
R 19
U 11
D 3
L 3
R 11
U 16
R 3
D 4
U 2
R 10
U 9
L 18
U 6
L 5
D 7
L 17
U 3
L 5
D 6
U 19
R 14
U 3
D 1
U 16
L 16
D 12
L 11
D 16
R 15
U 5
D 14
L 3
U 18
L 8
U 10
L 10
D 10
R 5|]