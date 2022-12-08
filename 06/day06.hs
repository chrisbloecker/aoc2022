#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.Set                          (Set)
import Data.Text                         (Text)
import Data.Void                         (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
--------------------------------------------------------------------------------
import qualified Data.Set         as S
import qualified Data.Text.IO     as T   (readFile)
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

parseInput :: Parser String
parseInput = many lowerChar <* eol <* eof

--------------------------------------------------------------------------------

findStart :: Int -> String -> Int
findStart k = go 0
    where
        go :: Int -> String -> Int
        go n xs = if S.size (S.fromList . take k $ xs) == k
                  then n + k
                  else go (n+1) (tail xs)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let filename = "input.txt"

    input <- parse parseInput filename <$> T.readFile filename

    case input of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right signal -> do
            putStrLn $ "Part 1: " ++ show (findStart  4 signal)
            putStrLn $ "Part 2: " ++ show (findStart 14 signal)