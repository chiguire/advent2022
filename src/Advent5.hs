{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Advent5
    ( advent5_1, advent5_2
    ) where

import Text.Heredoc
import Data.List
import Data.List.Split (splitOn, chunksOf)

-- Answers

advent5_1 = getTops $ moveCrates instructions state where
    (state, instructions) = parseInput input

advent5_2 = getTops $ moveCrates9001 instructions state where
    (state, instructions) = parseInput input

getTops :: [[Char]] -> String
getTops state = map head state

-- parsing

parseInput :: String -> ([[Char]], [Instruction])
parseInput ipt = (parseOriginalState originalState, parseInstructions instructions) where
    [originalState, instructions] = splitOn [""] $ lines ipt

parseOriginalState :: [String] -> [[Char]]
parseOriginalState ipt = crateLines where
    crateLines = map (reverse.unwords.words) $ filter (not.null.unwords.words) $ transpose $ map (parseCrateLine) $ tail reversedInput
    reversedInput = reverse ipt

parseCrateLine :: String -> [Char]
parseCrateLine l = map parseCrate $ chunksOf 4 l

parseCrate :: String -> Char
parseCrate (' ':_) = ' '
parseCrate ('[':c:_) = c

parseInstructions :: [String] -> [Instruction]
parseInstructions ipt = map (\x -> parseInstructions' $ words x) ipt where
    parseInstructions' ["move", m, "from", f, "to", t] = Instruction { getNBlocks = read m::Int, getFrom = read f::Int, getTo = read t::Int }

-- advent5_2
moveCrates9001 :: [Instruction] -> [[Char]] -> [[Char]]
moveCrates9001 instructions state = foldl moveInstruction9001 state instructions

moveInstruction9001 :: [[Char]] -> Instruction -> [[Char]]
moveInstruction9001 state i = pushWithBuffer buffer popped where
    pushWithBuffer [] s = s
    pushWithBuffer (c:buffer) s = pushWithBuffer buffer $ pushCrate to c s
    (popped, buffer) = popWithBuffer state
    popWithBuffer s = foldl (\(s', buffer) i -> (fst $ popCrate s' from, ((snd $ popCrate s' from):buffer))) (state, []) $ take n $ repeat i
    n = getNBlocks i
    from = getFrom i
    to = getTo i

-- advent5_1

moveCrates :: [Instruction] -> [[Char]] -> [[Char]]
moveCrates instructions state = foldl moveInstruction state instructions

moveInstruction :: [[Char]] -> Instruction ->  [[Char]]
moveInstruction state instruction = foldl (\s _ -> moveCrate (getFrom instruction) (getTo instruction) s) state $ take (getNBlocks instruction) $ repeat instruction

moveCrate :: Int -> Int -> [[Char]] -> [[Char]]
moveCrate from to l = pushCrate to c popped
    where
        (popped, c) = popCrate l from

popCrate :: [[Char]] -> Int -> ([[Char]], Char)
popCrate l from = popCrate' [] l from 1 ' ' where 
    popCrate' :: [[Char]] -> [[Char]] -> Int -> Int -> Char -> ([[Char]], Char)
    popCrate' l [] _ _ c = (reverse l, c)
    popCrate' l (x:xs) from idx c
        | from == idx = popCrate' ((tail x):l) xs from (from+1) (head x)
        | otherwise   = popCrate' (x:l) xs from (idx+1) c

pushCrate :: Int -> Char -> [[Char]] -> [[Char]]
pushCrate to c l = pushCrate' [] l to 1 c where
    pushCrate' l [] _ _ _ = reverse l
    pushCrate' l (x:xs) to idx c
        | to == idx = pushCrate' ((c:x):l) xs to (to+1) c
        | otherwise = pushCrate' (x:l) xs to (idx+1) c

data Instruction = Instruction {
    getNBlocks :: Int,
    getFrom :: Int,
    getTo :: Int
} deriving Show

-- Input

input_example = [here|    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2|]

input = [here|[H]                 [Z]         [J]
[L]     [W] [B]     [G]         [R]
[R]     [G] [S]     [J] [H]     [Q]
[F]     [N] [T] [J] [P] [R]     [F]
[B]     [C] [M] [R] [Q] [F] [G] [P]
[C] [D] [F] [D] [D] [D] [T] [M] [G]
[J] [C] [J] [J] [C] [L] [Z] [V] [B]
[M] [Z] [H] [P] [N] [W] [P] [L] [C]
 1   2   3   4   5   6   7   8   9 

move 3 from 2 to 1
move 8 from 6 to 4
move 4 from 8 to 2
move 3 from 1 to 9
move 1 from 2 to 4
move 3 from 7 to 5
move 3 from 9 to 2
move 3 from 3 to 5
move 1 from 5 to 1
move 5 from 1 to 8
move 2 from 1 to 8
move 3 from 7 to 3
move 1 from 8 to 9
move 6 from 9 to 8
move 3 from 8 to 7
move 7 from 8 to 9
move 2 from 5 to 9
move 2 from 2 to 9
move 3 from 3 to 7
move 2 from 8 to 3
move 7 from 4 to 8
move 3 from 4 to 1
move 4 from 8 to 6
move 4 from 6 to 1
move 8 from 1 to 2
move 1 from 1 to 4
move 3 from 5 to 1
move 8 from 9 to 8
move 4 from 3 to 1
move 5 from 5 to 3
move 2 from 7 to 1
move 1 from 7 to 4
move 1 from 7 to 2
move 3 from 3 to 5
move 3 from 9 to 1
move 9 from 8 to 1
move 2 from 9 to 7
move 1 from 8 to 5
move 4 from 5 to 3
move 1 from 3 to 4
move 1 from 9 to 6
move 1 from 6 to 9
move 7 from 4 to 9
move 1 from 7 to 3
move 1 from 8 to 2
move 8 from 2 to 1
move 4 from 3 to 5
move 2 from 9 to 6
move 2 from 6 to 2
move 2 from 4 to 9
move 8 from 9 to 2
move 3 from 7 to 9
move 1 from 3 to 5
move 2 from 3 to 8
move 9 from 2 to 1
move 1 from 8 to 7
move 4 from 2 to 9
move 4 from 5 to 6
move 1 from 8 to 9
move 27 from 1 to 2
move 1 from 6 to 4
move 3 from 6 to 4
move 7 from 9 to 8
move 4 from 4 to 1
move 9 from 2 to 6
move 2 from 1 to 9
move 6 from 1 to 3
move 1 from 5 to 3
move 3 from 3 to 5
move 3 from 5 to 3
move 3 from 3 to 1
move 4 from 6 to 7
move 3 from 9 to 2
move 1 from 6 to 4
move 4 from 3 to 5
move 3 from 6 to 5
move 1 from 6 to 2
move 15 from 2 to 3
move 5 from 5 to 9
move 13 from 3 to 9
move 2 from 5 to 7
move 1 from 4 to 2
move 3 from 3 to 7
move 11 from 2 to 7
move 7 from 9 to 5
move 3 from 5 to 7
move 6 from 8 to 9
move 4 from 1 to 2
move 6 from 1 to 6
move 3 from 5 to 1
move 1 from 8 to 2
move 4 from 2 to 9
move 1 from 5 to 7
move 6 from 7 to 6
move 18 from 7 to 5
move 1 from 7 to 1
move 8 from 9 to 5
move 1 from 2 to 6
move 15 from 5 to 6
move 6 from 5 to 3
move 4 from 3 to 6
move 26 from 6 to 5
move 2 from 1 to 7
move 4 from 5 to 9
move 8 from 5 to 7
move 3 from 7 to 9
move 14 from 9 to 8
move 7 from 5 to 2
move 4 from 2 to 1
move 5 from 1 to 9
move 12 from 5 to 3
move 5 from 8 to 5
move 14 from 3 to 2
move 1 from 5 to 2
move 10 from 2 to 6
move 7 from 9 to 6
move 6 from 8 to 6
move 1 from 2 to 7
move 2 from 9 to 7
move 2 from 8 to 6
move 6 from 2 to 7
move 1 from 1 to 8
move 15 from 6 to 2
move 1 from 6 to 9
move 1 from 5 to 9
move 1 from 9 to 6
move 2 from 2 to 4
move 3 from 9 to 5
move 5 from 5 to 3
move 3 from 3 to 6
move 6 from 2 to 7
move 1 from 5 to 9
move 8 from 6 to 9
move 2 from 6 to 7
move 3 from 2 to 4
move 9 from 6 to 7
move 17 from 7 to 5
move 1 from 8 to 4
move 7 from 9 to 3
move 12 from 5 to 8
move 3 from 5 to 2
move 4 from 7 to 8
move 2 from 5 to 7
move 1 from 7 to 9
move 8 from 3 to 7
move 17 from 7 to 5
move 3 from 2 to 5
move 1 from 3 to 6
move 10 from 5 to 4
move 5 from 2 to 7
move 1 from 4 to 2
move 3 from 9 to 8
move 7 from 7 to 2
move 5 from 5 to 1
move 14 from 4 to 9
move 3 from 9 to 8
move 1 from 6 to 9
move 2 from 1 to 4
move 2 from 8 to 5
move 16 from 8 to 6
move 1 from 6 to 2
move 11 from 9 to 2
move 2 from 7 to 5
move 1 from 1 to 6
move 11 from 2 to 9
move 4 from 2 to 8
move 9 from 5 to 3
move 1 from 4 to 2
move 2 from 1 to 8
move 1 from 2 to 9
move 2 from 4 to 3
move 8 from 6 to 9
move 16 from 9 to 3
move 16 from 3 to 2
move 17 from 2 to 6
move 1 from 9 to 3
move 1 from 2 to 5
move 1 from 9 to 4
move 3 from 2 to 8
move 1 from 9 to 1
move 1 from 9 to 6
move 7 from 3 to 1
move 5 from 3 to 5
move 3 from 8 to 3
move 2 from 3 to 4
move 6 from 8 to 4
move 7 from 6 to 4
move 3 from 6 to 7
move 3 from 8 to 9
move 3 from 5 to 2
move 3 from 1 to 3
move 1 from 4 to 8
move 3 from 5 to 1
move 13 from 4 to 7
move 14 from 6 to 7
move 6 from 1 to 9
move 3 from 9 to 6
move 1 from 8 to 7
move 1 from 8 to 7
move 20 from 7 to 3
move 1 from 8 to 9
move 1 from 1 to 9
move 1 from 1 to 5
move 1 from 4 to 6
move 14 from 3 to 9
move 1 from 2 to 6
move 3 from 7 to 6
move 6 from 3 to 2
move 1 from 3 to 8
move 2 from 7 to 3
move 7 from 6 to 3
move 12 from 3 to 1
move 1 from 8 to 2
move 1 from 4 to 9
move 1 from 5 to 6
move 1 from 6 to 4
move 1 from 4 to 2
move 2 from 2 to 3
move 16 from 9 to 7
move 3 from 6 to 7
move 6 from 9 to 4
move 4 from 4 to 7
move 6 from 1 to 8
move 2 from 3 to 6
move 3 from 1 to 9
move 3 from 2 to 3
move 3 from 3 to 8
move 5 from 2 to 8
move 2 from 7 to 8
move 3 from 1 to 5
move 1 from 4 to 3
move 2 from 9 to 8
move 1 from 6 to 8
move 2 from 9 to 1
move 15 from 7 to 1
move 1 from 6 to 5
move 10 from 1 to 5
move 1 from 4 to 1
move 2 from 1 to 6
move 9 from 7 to 8
move 27 from 8 to 3
move 1 from 6 to 1
move 1 from 8 to 5
move 5 from 5 to 6
move 12 from 3 to 1
move 3 from 7 to 1
move 7 from 5 to 1
move 1 from 6 to 4
move 3 from 6 to 9
move 1 from 4 to 2
move 2 from 6 to 5
move 1 from 7 to 6
move 1 from 9 to 2
move 2 from 5 to 6
move 2 from 6 to 5
move 3 from 1 to 3
move 19 from 3 to 1
move 2 from 2 to 9
move 42 from 1 to 7
move 4 from 9 to 7
move 1 from 6 to 8
move 1 from 8 to 5
move 2 from 1 to 9
move 3 from 5 to 7
move 27 from 7 to 4
move 1 from 1 to 4
move 3 from 9 to 2
move 18 from 4 to 9
move 2 from 5 to 3
move 1 from 7 to 1
move 2 from 3 to 4
move 8 from 7 to 5
move 15 from 9 to 3
move 1 from 9 to 7
move 3 from 7 to 2
move 2 from 7 to 2
move 2 from 5 to 3
move 1 from 1 to 5
move 1 from 9 to 1
move 1 from 3 to 1
move 1 from 4 to 3
move 8 from 7 to 3
move 8 from 2 to 4
move 1 from 9 to 6
move 23 from 3 to 9
move 1 from 9 to 6
move 2 from 6 to 8
move 1 from 8 to 6
move 1 from 5 to 3
move 7 from 4 to 8
move 7 from 5 to 7
move 2 from 8 to 3
move 1 from 1 to 8
move 3 from 7 to 4
move 5 from 4 to 3
move 1 from 1 to 8
move 3 from 3 to 1
move 8 from 9 to 7
move 3 from 8 to 4
move 1 from 6 to 2
move 5 from 8 to 7
move 6 from 3 to 1
move 1 from 2 to 9
move 7 from 7 to 9
move 4 from 1 to 9
move 2 from 4 to 2
move 1 from 4 to 9
move 1 from 1 to 6
move 8 from 4 to 8
move 4 from 1 to 5
move 3 from 5 to 2
move 2 from 2 to 5
move 2 from 5 to 6
move 1 from 3 to 7
move 2 from 6 to 4
move 1 from 5 to 7
move 1 from 6 to 9
move 1 from 4 to 1
move 6 from 9 to 2
move 8 from 9 to 7
move 4 from 7 to 3
move 4 from 8 to 3
move 3 from 8 to 3
move 8 from 3 to 5
move 1 from 1 to 7
move 11 from 9 to 7
move 5 from 2 to 7
move 1 from 8 to 1
move 3 from 2 to 3
move 1 from 1 to 4
move 1 from 2 to 5
move 20 from 7 to 8
move 7 from 7 to 9
move 4 from 4 to 7
move 3 from 9 to 4
move 5 from 7 to 4
move 7 from 4 to 7
move 4 from 9 to 2
move 1 from 4 to 3
move 4 from 3 to 5
move 2 from 5 to 8
move 4 from 5 to 2
move 5 from 2 to 6
move 2 from 6 to 3
move 22 from 8 to 5
move 13 from 7 to 9
move 11 from 9 to 3
move 2 from 6 to 8
move 7 from 3 to 1
move 18 from 5 to 2
move 1 from 6 to 4
move 1 from 4 to 9
move 2 from 8 to 5
move 2 from 9 to 1
move 9 from 3 to 1
move 4 from 5 to 6
move 2 from 6 to 7
move 3 from 9 to 5
move 10 from 5 to 8
move 6 from 8 to 7
move 3 from 8 to 1
move 6 from 2 to 3
move 1 from 9 to 6
move 5 from 3 to 4
move 4 from 1 to 4
move 17 from 1 to 5
move 12 from 2 to 7
move 1 from 3 to 6
move 16 from 5 to 8
move 3 from 5 to 6
move 9 from 8 to 3
move 8 from 8 to 4
move 7 from 4 to 1
move 5 from 1 to 4
move 4 from 3 to 7
move 14 from 7 to 3
move 6 from 4 to 8
move 9 from 7 to 4
move 5 from 6 to 1
move 1 from 7 to 1
move 1 from 6 to 7
move 16 from 4 to 5
move 1 from 4 to 2
move 1 from 7 to 5
move 2 from 1 to 7
move 2 from 7 to 4
move 4 from 1 to 6
move 13 from 5 to 6
move 5 from 6 to 3
move 22 from 3 to 2
move 1 from 4 to 7
move 4 from 5 to 4
move 1 from 7 to 6
move 5 from 8 to 5
move 2 from 3 to 1
move 13 from 6 to 1
move 6 from 1 to 4
move 1 from 8 to 1
move 6 from 1 to 4
move 1 from 5 to 4
move 7 from 4 to 7
move 3 from 1 to 5
move 2 from 5 to 7
move 5 from 5 to 1
move 8 from 7 to 4
move 1 from 6 to 4
move 1 from 7 to 4
move 9 from 2 to 7
move 8 from 7 to 6
move 5 from 6 to 4
move 1 from 7 to 4
move 2 from 4 to 9
move 2 from 6 to 1
move 8 from 2 to 6
move 9 from 1 to 8
move 9 from 6 to 2
move 1 from 1 to 8
move 6 from 8 to 4
move 2 from 9 to 7
move 2 from 7 to 9
move 15 from 2 to 8
move 18 from 4 to 2
move 14 from 4 to 5
move 10 from 2 to 4
move 9 from 2 to 6
move 1 from 9 to 3
move 1 from 3 to 1
move 6 from 5 to 8
move 3 from 4 to 9
move 2 from 2 to 1
move 1 from 1 to 6
move 3 from 9 to 7
move 22 from 8 to 6
move 1 from 8 to 9
move 2 from 1 to 5
move 5 from 5 to 4
move 2 from 5 to 8
move 2 from 8 to 7
move 1 from 9 to 7
move 1 from 5 to 8
move 1 from 9 to 8
move 15 from 6 to 4
move 2 from 5 to 2
move 11 from 4 to 6
move 5 from 4 to 1
move 5 from 4 to 2
move 2 from 1 to 5
move 6 from 2 to 8
move 11 from 6 to 3
move 12 from 6 to 8
move 1 from 3 to 9
move 3 from 3 to 2
move 6 from 4 to 2
move 2 from 5 to 8
move 5 from 7 to 2
move 11 from 8 to 4
move 1 from 7 to 4
move 1 from 9 to 6
move 7 from 2 to 1
move 3 from 6 to 5
move 2 from 5 to 3
move 1 from 5 to 9
move 3 from 4 to 9
move 4 from 9 to 1
move 4 from 3 to 6
move 3 from 4 to 8
move 3 from 8 to 9
move 2 from 8 to 2
move 9 from 8 to 7
move 2 from 3 to 1
move 2 from 3 to 2
move 1 from 3 to 6
move 2 from 9 to 1
move 8 from 7 to 5
move 7 from 2 to 7
move 2 from 8 to 9
move 4 from 6 to 5
move 13 from 1 to 5
move 4 from 1 to 8
move 3 from 9 to 3
move 12 from 5 to 9
move 3 from 8 to 9
move 1 from 8 to 4
move 3 from 2 to 7
move 3 from 3 to 7
move 1 from 9 to 2
move 4 from 6 to 4
move 6 from 5 to 6
move 2 from 7 to 3
move 2 from 2 to 1
move 5 from 6 to 5
move 1 from 1 to 7
move 9 from 5 to 4
move 10 from 9 to 6
move 1 from 2 to 6
move 12 from 7 to 6
move 1 from 7 to 4
move 23 from 6 to 1
move 10 from 4 to 3
move 16 from 1 to 5
move 5 from 1 to 2
move 6 from 3 to 7
move 5 from 4 to 8|]