module Main where

import Data.Char (digitToInt, isDigit)
import Data.List (stripPrefix)

input = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"

wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

spelling =
  [ ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)
  ]

replaceSingle [] word = word
replaceSingle (r : rs) word = if word == fst r then snd r else replaceSingle rs word

main = do
  --   input <- readFile "app/src/1.txt"
  print $ sum (map ((\s -> read (head s : last s : "") :: Integer) . filter isDigit) (wordsWhen (== '\n') input))
