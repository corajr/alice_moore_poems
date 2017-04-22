#!/usr/bin/env stack
-- stack --resolver lts-8.11 --install-ghc runghc --package text
{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (maximumBy)
import Data.Function (on)

main :: IO ()
main = getArgs >>= mapM_ doAddLabel


doAddLabel :: FilePath -> IO ()
doAddLabel inFile = do
  let outName = takeWhile (/= '.') . drop 6 $ inFile
  input <- T.readFile inFile
  let out = addLabel (T.pack outName) (T.lines input)
  T.writeFile inFile (T.unlines out)

addLabel :: Text -> [Text] -> [Text]
addLabel name input = begin ++ ["\\label{ch:" <> name <> "}"] ++ end
  where
    (begin, end) = splitAt 1 input

process :: FilePath -> IO ()
process x = do
  let outFile = "poems/" ++ (iterate init . drop 1 . dropWhile (/= '/') $ x) !! 4 ++ ".tex"
  input <- readFile x
  let out = procPoem (lines input)
  writeFile outFile (unlines out)

procPoem :: [String] -> [String]
procPoem (x:_:xs) = title : versewidth : beginVerse : addPar (map clean xs) ++ [endVerse]
  where
    title = "\\PoemTitle{" ++ x ++ "}"
    width = maximumBy (compare `on` length) xs
    versewidth = "\\settowidth{\\versewidth}{" ++ width ++ "}"
    beginVerse = "\\begin{verse}[\\versewidth]"
    endVerse = "\\end{verse}"
    clean = concatMap cleanChar
    cleanChar '—' = "---"
    cleanChar '‘' = "'"
    cleanChar '’' = "'"
    cleanChar '“' = "``"
    cleanChar '”' = "''"
    cleanChar '&' = "\\&"
    cleanChar '\t' = " \\qquad "
    cleanChar x = [x]

addPar :: [String] -> [String]
addPar (x:xs@(y:xs')) = if y == "" then x:y:addPar xs' else (x ++ "\\\\"):addPar xs
addPar xs = xs
