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

data Move     = Rock | Paper | Scissor deriving (Show)
type Guide    = (Move, Move)
type Strategy = [Guide]

parseInput :: Parser Strategy
parseInput = many (guide <* eol) <* eof
  where
    guide :: Parser Guide
    guide = do
        m1 <- move
        char ' '
        m2 <- move
        return (m1,m2)

    move :: Parser Move
    move = choice [ char 'A' >> return Rock
                  , char 'B' >> return Paper
                  , char 'C' >> return Scissor
                  , char 'X' >> return Rock
                  , char 'Y' >> return Paper
                  , char 'Z' >> return Scissor
                  ]


value :: Move -> Move -> Integer
value Rock    Rock    = 1 + 3
value Rock    Paper   = 2 + 6
value Rock    Scissor = 3 + 0
value Paper   Rock    = 1 + 0
value Paper   Paper   = 2 + 3
value Paper   Scissor = 3 + 6
value Scissor Rock    = 1 + 6
value Scissor Paper   = 2 + 0
value Scissor Scissor = 3 + 3

whichMove :: Move -> Move -> Guide
whichMove Rock    Rock    = (Rock, Scissor)
whichMove Rock    Paper   = (Rock, Rock)
whichMove Rock    Scissor = (Rock, Paper)
whichMove Paper   Rock    = (Paper, Rock)
whichMove Paper   Paper   = (Paper, Paper)
whichMove Paper   Scissor = (Paper, Scissor)
whichMove Scissor Rock    = (Scissor, Paper)
whichMove Scissor Paper   = (Scissor, Scissor)
whichMove Scissor Scissor = (Scissor, Rock)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let filename = "input.txt"

    input <- parse parseInput filename <$> T.readFile filename

    case input of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right strategy -> do
            putStrLn $ "Part 1: " ++ show (sum . map (uncurry value) $ strategy)
            putStrLn $ "Part 2: " ++ show (sum . map (uncurry value) . map (uncurry whichMove) $ strategy)