#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main
  where
--------------------------------------------------------------------------------
import Data.Char                         (ord)
import Data.Set                          (Set, intersection, union, fromList, toList)
import Data.Text                         (Text)
import Data.Void                         (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
--------------------------------------------------------------------------------
import qualified Data.Text.IO     as T   (readFile)
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

type Item     = Char
type Backpack = (Set Item, Set Item)

parseInput :: Parser [Backpack]
parseInput = many backpack <* eof
  where
    backpack :: Parser Backpack
    backpack = do
      items <- many letterChar <* eol
      let len = length items `div` 2
      return (fromList . take len $ items, fromList . drop len $ items)

--------------------------------------------------------------------------------

overlap :: Backpack -> Set Item
overlap = uncurry intersection

value :: [Item] -> Integer
value = fromIntegral . sum . map f
  where
    f :: Item -> Int
    f i | 'a' <= i && i <= 'z' = ord i - ord 'a' + 1
        | 'A' <= i && i <= 'Z' = ord i - ord 'A' + 27
        | otherwise            = error $ "Unexpected item: " ++ [i]

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n l  = take n l : chunks n (drop n l)

badge :: [Backpack] -> Set Item
badge [(c1,c2)   ] = c1 `union` c2
badge ((c1,c2):bs) = c1 `union` c2 `intersection` badge bs

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let filename = "input.txt"

    input <- parse parseInput filename <$> T.readFile filename

    case input of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right backpacks -> do
            putStrLn $ "Part 1: " ++ show (sum . map (value . toList . overlap) $ backpacks)
            putStrLn $ "Part 2: " ++ show (value . map (head . toList . badge) . chunks 3 $ backpacks)