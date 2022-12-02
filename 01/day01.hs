#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main
  where
--------------------------------------------------------------------------------
import Data.List                         (sort)
import Data.Text                         (Text)
import Data.Void                         (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
--------------------------------------------------------------------------------
import qualified Data.Text.IO     as T   (readFile)
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

type Elf = [Integer]

parseInput :: Parser [Elf]
parseInput = sepBy (many (decimal <* eol)) eol

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let filename = "input.txt"

    input <- parse parseInput filename <$> T.readFile filename

    case input of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right elves -> do
            putStrLn $ "Part 1: " ++ show (maximum . map sum $ elves)
            putStrLn $ "Part 2: " ++ show (sum . take 3 . reverse . sort . map sum $ elves)