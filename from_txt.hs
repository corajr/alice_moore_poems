#!/usr/bin/env stack
-- stack --resolver lts-8.11 --install-ghc runghc

import System.Environment (getArgs)
import Data.List (maximumBy)
import Data.Function (on)

main :: IO ()
main = getArgs >>= mapM_ process

process :: String -> IO ()
process x = do
  let outFile = "poems/" ++ (iterate init . drop 1 . dropWhile (/= '/') $ x) !! 4 ++ ".tex"
  input <- readFile x
  let out = procPoem (lines input)
  writeFile outFile (unlines out)

procPoem :: [String] -> [String]
procPoem (x:_:xs) = title : versewidth : beginVerse : addPar xs ++ [endVerse]
  where
    title = "\\PoemTitle{" ++ x ++ "}"
    width = maximumBy (compare `on` length) xs
    versewidth = "\\settowidth{\\versewidth}{" ++ width ++ "}"
    beginVerse = "\\begin{verse}[\\versewidth]"
    endVerse = "\\end{verse}"

addPar :: [String] -> [String]
addPar (x:xs@(y:xs')) = if y == "" then x:y:addPar xs' else (x ++ "\\\\"):addPar xs
addPar xs = xs
