module Main where

import Data.Char (digitToInt, isDigit)
import Data.List (stripPrefix)

wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

main = do
  input <- readFile "app/src/1.txt"
  print $ sum (map ((\s -> read (head s : last s : "") :: Integer) . filter isDigit) (wordsWhen (== '\n') input))
