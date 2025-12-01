{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import System.Environment (getArgs)

import Day01 qualified as D1

main :: IO ()
main = do
  args <- getArgs
  case read <$> args of
    (1 : _) -> D1.day
