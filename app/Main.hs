module Main where

import Data.List (stripPrefix)

input = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

main = print $ wordsWhen (== '\n') input
