#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.Functor                      ((<&>))
import Data.List                         (sort)
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

data FileSystem = Directory String [FileSystem]
                | File String Int
    deriving (Show)

type Bash = [Command]

data Command = CD String
             | LS [Entry]
    deriving (Show)

data Entry = FileEntry String Int
           | DirectoryEntry String
    deriving (Show)

parseInput :: Parser Bash
parseInput = many command <* eof
    where
        command :: Parser Command
        command = choice [ string "$ cd " *> many (satisfy (/= '\n')) <* eol >>= \d -> return (CD d)
                         , string "$ ls" *> eol *> many lsOutput >>= \xs -> return (LS xs)
                         ]
        
        lsOutput :: Parser Entry
        lsOutput = choice [ directory
                          , file
                          ]

        directory :: Parser Entry
        directory = string "dir " *> many letterChar <* eol <&> DirectoryEntry

        file :: Parser Entry
        file = decimal >>= \size -> char ' ' *> many (satisfy (/= '\n')) <* eol >>= \name -> return (FileEntry name size)

--------------------------------------------------------------------------------

isFileEntry :: Entry -> Bool
isFileEntry (FileEntry _ _) = True
isFileENtry (DirectoryEntry _) = False

buildFileSystem :: Bash -> FileSystem
buildFileSystem (CD "/":bs) = let ([], fs) = go bs (Directory "/" []) in fs
    where
        go :: Bash -> FileSystem -> (Bash, FileSystem)
        go [] fs = ([], fs)
        go (LS es:bs) (Directory directoryName entries) = let fileEntries = filter isFileEntry es
                                                              fs = Directory directoryName (entries ++ [ File filename size | FileEntry filename size <- es ])
                                                          in go bs fs
        go (CD "..":bs) fs = (bs, fs)
        go (CD s:bs) (Directory directoryName entries) = let (bs', fs) = go bs (Directory s [])
                                                         in go bs' (Directory directoryName (entries ++ [fs]))
        go (b:bs) _ = error (show b)
buildFileSystem _ = error "We really want to start at /."

dirSize :: FileSystem -> (String, Int)
dirSize = \case
              d@(Directory name _) -> (name, go d)
              (File name size)     -> (name, size)
    where
        go (Directory _ es) = sum . map go $ es
        go (File _ size) = size

listDirSizes :: FileSystem -> [(String, Int)]
listDirSizes d@(Directory _ entries) = dirSize d : concatMap listDirSizes (concatMap (\case {(File _ _) -> []; d@(Directory _ _) -> [d]}) entries)

listDirSizes' :: FileSystem -> [(String, Int)]
listDirSizes' = go
    where
        go :: FileSystem -> [(String, Int)]
        go (Directory directoryName entries) = let fileSizes      = concatMap (\case {f@(File _ _) -> go f; (Directory _ _) -> []}) entries
                                                   directorySizes = concatMap (\case {(File _ _) -> []; d@(Directory _ _) -> go d}) entries
                                               in (directoryName, sum (map snd $ fileSizes ++ directorySizes)) : directorySizes
        go (File filename size) = [(filename, size)]

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let filename = "input.txt"

    input <- parse parseInput filename <$> T.readFile filename

    case input of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right bash -> do
            let fs       = buildFileSystem bash
                dirSizes = map snd . listDirSizes $ fs
                largest  = head dirSizes
            putStrLn $ "Part 1: " ++ show (sum . filter (<= 100000) $ dirSizes)
            putStrLn $ "Part 2: " ++ show (head . filter (\s -> largest - s <= 40000000) . sort $ dirSizes)