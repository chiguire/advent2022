{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Advent11
    ( advent11_1, advent11_2
    ) where

import Text.Heredoc

-- Answers

advent11_1 = 0

advent11_2 = 0

-- Input

input_example = [here|Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1|]

input = [here|Monkey 0:
  Starting items: 54, 82, 90, 88, 86, 54
  Operation: new = old * 7
  Test: divisible by 11
    If true: throw to monkey 2
    If false: throw to monkey 6

Monkey 1:
  Starting items: 91, 65
  Operation: new = old * 13
  Test: divisible by 5
    If true: throw to monkey 7
    If false: throw to monkey 4

Monkey 2:
  Starting items: 62, 54, 57, 92, 83, 63, 63
  Operation: new = old + 1
  Test: divisible by 7
    If true: throw to monkey 1
    If false: throw to monkey 7

Monkey 3:
  Starting items: 67, 72, 68
  Operation: new = old * old
  Test: divisible by 2
    If true: throw to monkey 0
    If false: throw to monkey 6

Monkey 4:
  Starting items: 68, 89, 90, 86, 84, 57, 72, 84
  Operation: new = old + 7
  Test: divisible by 17
    If true: throw to monkey 3
    If false: throw to monkey 5

Monkey 5:
  Starting items: 79, 83, 64, 58
  Operation: new = old + 6
  Test: divisible by 13
    If true: throw to monkey 3
    If false: throw to monkey 0

Monkey 6:
  Starting items: 96, 72, 89, 70, 88
  Operation: new = old + 4
  Test: divisible by 3
    If true: throw to monkey 1
    If false: throw to monkey 2

Monkey 7:
  Starting items: 79
  Operation: new = old + 8
  Test: divisible by 19
    If true: throw to monkey 4
    If false: throw to monkey 5|]