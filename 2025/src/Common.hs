{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common (runDay, Parser) where

import Control.Exception (IOException, handle)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Void
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, errorBundlePretty, parse)
import Text.Printf

type Parser = Parsec Void Text

fmtDay :: Int -> String
fmtDay = printf "%02d"

parseFile :: String -> Parser a -> IO a
parseFile fileName parser = do
  input <- Text.readFile fileName
  case parse parser fileName input of
    Left err -> do
      putStrLn $ errorBundlePretty err
      exitFailure
    Right a -> pure a

runDay :: (Show b, Show c) => Int -> Parser a -> (a -> b) -> (a -> c) -> IO ()
runDay number parser p1 p2 = do
  handle (\(_ :: IOException) -> putStrLn "No sample") $ do
    let inFile = "inputs/day" <> fmtDay number <> "-sample.txt"
    input <- parseFile inFile parser
    putStr "Part 1 sample: "
    print $ p1 input
    putStr "Part 2 sample: "
    print $ p2 input
  let inFile = "inputs/day" <> fmtDay number <> ".txt"
  input <- parseFile inFile parser
  putStr "Part 1 real: "
  print $ p1 input
  putStr "Part 2 real: "
  print $ p2 input
