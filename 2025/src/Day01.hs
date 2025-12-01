module Day01 (day) where

import Common (Parser, runDay)
import Control.Applicative (Alternative ((<|>)), many)
import Data.Bool (bool)
import Data.Functor (($>))
import Text.Megaparsec (single)
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (decimal)

data Turn = TLeft Int | TRight Int

day :: IO ()
day = runDay 1 parse solve1 (solve2 0)

parse :: Parser [Turn]
parse = many $ ((single 'L' $> TLeft <|> single 'R' $> TRight) <*> decimal) <* space

zeros :: [Int] -> Int
zeros = length . filter (== 0)

solve1 :: [Turn] -> Int
solve1 = zeros . scanl step 50 . fmap offset
 where
  step acc n = (acc + 10000 + n) `mod` 100
  offset (TLeft n) = negate n
  offset (TRight n) = n

bound :: Int -> Int
bound n = (n + 100) `mod` 100

solve2 :: Int -> [Turn] -> Int
solve2 _ [] = 0
solve2 pos (TLeft 0 : xs) = solve2 pos xs
solve2 pos (TLeft n : xs) = solve2 (bound $ pred pos) (TLeft (pred n) : xs) + bool 0 1 (pos == 1)
solve2 pos (TRight 0 : xs) = solve2 pos xs
solve2 pos (TRight n : xs) = solve2 (bound $ succ pos) (TRight (pred n) : xs) + bool 0 1 (pos == 99)
