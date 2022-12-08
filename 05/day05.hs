#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.List                         (sort)
import Data.Map                          (Map)
import Data.Maybe                        (Maybe(..), mapMaybe, catMaybes)
import Data.Text                         (Text)
import Data.Void                         (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
--------------------------------------------------------------------------------
import qualified Data.Map         as M
import qualified Data.Text.IO     as T   (readFile)
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

type Crate = Char
type Stack = [Crate]

data Instruction = Instruction { amount :: Int
                               , source :: Int
                               , target :: Int
                               }
    deriving (Show)

parseInput :: Parser (Map Int Stack, [Instruction])
parseInput = do
    ss <- mkStacks <$> stacks
    eol
    is <- many (instruction <* eol)
    eof
    return (ss, is)

    where
        stacks :: Parser [[Maybe Crate]]
        stacks = many (sepBy crate (char ' ') <* eol) <* stackLabels

        mkStacks :: [[Maybe Crate]] -> Map Int Stack
        mkStacks ss = go ss 1 M.empty
          where
            go :: [[Maybe Crate]] -> Int -> Map Int Stack -> Map Int Stack
            go ([]:_) _ m = m
            go ss     n m = let m' = M.insert n (mapMaybe head ss) m 
                            in go (map tail ss) (n+1) m'
        
        crate :: Parser (Maybe Crate)
        crate = choice [ string "   " >> return Nothing
                       , char '[' *> letterChar <* char ']' >>= return . Just
                       ]

        stackLabels :: Parser [Int]
        stackLabels = sepBy (char ' ' *> decimal <* char ' ') (char ' ') <* eol

        instruction :: Parser Instruction
        instruction = do
            string "move "
            amount <- decimal
            string " from "
            source <- decimal
            string " to "
            target <- decimal
            return Instruction{..}

--------------------------------------------------------------------------------

runInstruction :: (Stack -> Stack) -> Instruction -> Map Int Stack -> Map Int Stack
runInstruction f Instruction{..} m =
    let s  = m M.! source
        t  = m M.! target
        s' = drop amount s
        t' = f (take amount s) ++ t
    in M.fromList [(source, s'), (target, t')] `M.union` m

getMessage :: Map Int Stack -> String
getMessage = concatMap (take 1 . snd) . sort . M.toList

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let filename = "input.txt"

    input <- parse parseInput filename <$> T.readFile filename

    case input of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right (stacks, instructions) -> do
            putStrLn $ "Part 1: " ++ show (getMessage . foldl (flip (runInstruction reverse)) stacks $ instructions)
            putStrLn $ "Part 1: " ++ show (getMessage . foldl (flip (runInstruction id))      stacks $ instructions)