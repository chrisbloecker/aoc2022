#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main
  where
--------------------------------------------------------------------------------
import Data.Text                         (Text)
import Data.Void                         (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
--------------------------------------------------------------------------------
import qualified Data.Text.IO     as T   (readFile)
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

type Interval = (Int, Int)
type Entry    = (Interval, Interval)

parseInput :: Parser [Entry]
parseInput = many (entry <* eol) <* eof
  where
    entry :: Parser Entry
    entry = do 
      i1 <- interval
      char ','
      i2 <- interval
      return (i1, i2)

    interval :: Parser Interval
    interval = do
      start <- decimal
      char '-'
      end   <- decimal
      return (start, end)

--------------------------------------------------------------------------------

-- whether the first interval contains the second
contains :: Interval -> Interval -> Bool
contains (s1, e1) (s2, e2) = s1 <= s2 && e2 <= e1

-- whether the two intervals overlap
overlap :: Interval -> Interval -> Bool
overlap (s1, e1) (s2, e2) = s1 <= s2 && s2 <= e1
                         || s1 <= e2 && e2 <= e1
                         || s2 <= s1 && s1 <= e2
                         || s2 <= e1 && e1 <= e2

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let filename = "input.txt"

    input <- parse parseInput filename <$> T.readFile filename

    case input of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right entries -> do
            putStrLn $ "Part 1: " ++ show (length . filter (\e -> (uncurry contains) e || uncurry (flip contains) e) $ entries)
            putStrLn $ "Part 2: " ++ show (length . filter (uncurry overlap) $ entries)