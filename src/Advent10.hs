{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Advent10
    ( advent10_1, advent10_2, valuesList, input_example, input_example2, parseInput, executeInstructions
    ) where

import Data.List.Split (chunksOf)
import Data.Foldable

import Text.Heredoc

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator (manyTill)

-- Answers

advent10_1 = sumCycles . executeInstructions
         <$> parse parseInput "" input

advent10_2 = renderScreen . executeInstructions
         <$> parse parseInput "" input

-- Render

renderScreen :: [Int] -> [String]
renderScreen = words . concatMap (drawPixel) . zip [1..]

drawPixel :: (Int, Int) -> String
drawPixel (cycle, x) = isInPixel (cycle, x) : (aNewline cycle) where
    aNewline c
        | c `elem` [40, 80, 120, 160, 200, 240] = "\n"
        | otherwise = ""
    isInPixel (cycle, x)
        | abs (cInLine - x - 1) <= 1 = '#'
        | otherwise = '.' where
            cInLine = 1 + (cycle-1) `mod` 40

-- Execution

executeInstructions :: [Instruction] -> [Int]
executeInstructions = fst
                    . foldl' (computerCycle) ([], 1)

startingState = (1,0)

computerCycle :: ([Int], Int) -> Instruction -> ([Int], Int)
computerCycle (x,c) Noop = (x ++ [c],c)
computerCycle (x,c) (Addx v) = (x ++ [c,c],c+v)

valuesList :: [(Int, Int)] -> [Int]
valuesList = concatMap (\(x,c) -> take c $ repeat x)

sumCycles :: [Int] -> Int
sumCycles cycles = sum $ map (cycleValue cycles) [20, 60, 100, 140, 180, 220]

cycleValue :: [Int] -> Int -> Int
cycleValue l c = let v = head $ drop (c-1) l in v * c

-- Parsing

parseInput = parseInstruction `sepBy` endOfLine

parseInstruction = try parseNoop <|> parseAddx

parseNoop = do
    string "noop"
    return Noop

parseAddx = try parseNegativeAddx <|> parsePositiveAddx

parseNegativeAddx = do
    string "addx -"
    number <- many digit
    return $ Addx $ (-1)*(read number)

parsePositiveAddx = do
    string "addx "
    number <- many digit
    return $ Addx $ read number

data Instruction = Noop | Addx Int deriving (Eq, Show)
-- Input

input_example2 = [here|noop
addx 3
addx -5|]

input_example = [here|addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop|]

input = [here|addx 1
addx 5
addx -1
addx 20
addx -14
addx -1
addx 5
addx 13
addx -12
addx 3
addx 3
addx 3
addx 1
addx 4
noop
noop
addx 1
noop
noop
addx 4
noop
addx -35
addx 11
addx -1
addx -7
addx 5
addx 2
addx 3
addx -2
addx 2
addx 5
addx 5
noop
noop
addx -2
addx 2
noop
addx 3
addx 2
addx 7
noop
noop
addx 3
addx -2
addx -36
noop
addx 25
addx -22
addx 7
noop
addx -2
noop
noop
noop
addx 5
addx 5
addx 4
noop
addx -2
addx 5
addx -4
addx 5
addx 4
noop
addx -29
addx 32
addx -23
addx -12
noop
addx 7
noop
addx -2
addx 4
addx 3
addx 20
addx 3
addx -20
addx 5
addx 16
addx -15
addx 6
noop
noop
noop
addx 5
noop
addx 5
noop
noop
noop
addx -37
addx 2
addx -2
addx 7
noop
addx -2
addx 5
addx 2
addx 3
addx -2
addx 2
addx 5
addx 2
addx -6
addx -15
addx 24
addx 2
noop
addx 3
addx -8
addx 15
addx -14
addx 15
addx -38
noop
noop
addx 21
addx -14
addx 1
addx 5
noop
addx -2
addx 7
addx -1
addx 5
noop
addx 2
addx 3
addx 3
addx -2
addx 4
addx 2
addx -17
addx 20
noop
noop
noop
noop|]