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
main = getArgs >>= mapM_ process

process :: FilePath -> IO ()
process x = do
  let name = (iterate init . drop 1 . dropWhile (/= '/') $ x) !! 4
      outFile = "poems/" ++ name ++ ".tex"
  input <- T.readFile x
  let out = procPoem name (T.lines input)
  T.writeFile outFile (T.unlines out)

procPoem :: String -> [Text] -> [Text]
procPoem name (x:_:xs) = title : label : versewidth : beginVerse : addPar (map clean xs) ++ [endVerse]
  where
    title = "\\PoemTitle{" <> x <> "}"
    label = "\\label{ch:" <> T.pack name <> "}"
    width = maximumBy (compare `on` T.length) xs
    versewidth = "\\settowidth{\\versewidth}{" <> width <> "}"
    beginVerse = "\\begin{verse}[\\versewidth]"
    endVerse = "\\end{verse}"
    clean = T.concatMap cleanChar
    cleanChar '—' = "---"
    cleanChar '‘' = "`"
    cleanChar '’' = "'"
    cleanChar '“' = "``"
    cleanChar '”' = "''"
    cleanChar '&' = "\\&"
    cleanChar '\t' = " \\qquad "
    cleanChar x = T.singleton x
procPoem name xs = error $ name <> "\n###\n" <> (T.unpack $ T.unlines xs)

addPar :: [Text] -> [Text]
addPar (x:xs@(y:xs')) = if y == "" then x:y:addPar xs' else (x <> "\\\\"):addPar xs
addPar xs = xs
